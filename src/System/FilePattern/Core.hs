{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

-- | This library supports patterns containing @*@ and @**@, but also
--   \"legacy\" patterns including @\/\/@ as well.
--   To support that, we have 'with' patterns that are customized by the lexer.
module System.FilePattern.Core(
    -- * Primitive API, as exposed
    FilePattern, matchBoolWith, matchWith,
    -- * Optimisation opportunities
    simpleWith,
    -- * Multipattern file rules
    compatibleWith, substituteWith,
    -- * Accelerated searching
    Walk(..), walkWith
    ) where

import Control.Exception.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Prelude
import System.FilePattern.Type
import System.FilePath (isPathSeparator)


---------------------------------------------------------------------
-- PATTERNS

-- | Optimisations that may change the matched expressions
optimise :: [Pat] -> [Pat]
optimise (Skip:Skip:xs) = optimise $ Skip:xs
optimise (x:xs) = x : optimise xs
optimise [] = []


-- | Given a pattern, and a list of path components, return a list of all matches
--   (for each wildcard in order, what the wildcard matched).
match :: [Pat] -> [String] -> [[String]]
match (Skip:xs) (y:ys) =
    map ("":) (match xs (y:ys)) ++
    [(y++"/"++r):rs | r:rs <- match (Skip:xs) ys]
match (Skip:xs) [] = map ("":) $ match xs []
match (Stars x:xs) (y:ys) | Just rs <- wildcard x y = map (rs ++) $ match xs ys
match [] [] = [[]]
match _ _ = []

repats :: Pats -> Wildcard [Wildcard String]
repats (Pats xs) = case map (map unstars) $ split (== Skip) xs of
    [] -> error "repats: given empty string"
    [x] -> Literal x
    pre:xs -> case unsnoc xs of
        Nothing -> error "repats: Stars check failed"
        Just (mid, post) -> Wildcard pre mid post
    where unstars (Stars x) = x

matchRepats :: Wildcard [Wildcard String] -> [String] -> Bool
matchRepats (Literal xs) ys = eq xs ys
matchRepats (Wildcard pre mid post) ys
    | length ys < sum (map length $ pre : post : mid) = False
    | otherwise = eq pre pre' && eq post post' && find mid rest2
        where
            (pre',rest) = splitAt (length pre) ys
            (rest2,post') = splitAtEnd (length post) rest

            find [] _ = True
            find (m:ms) (y:ys)
                | let (a,b) = splitAt (length m) (y:ys), eq m a = find ms b
                | otherwise = find (m:ms) ys
            find _ [] = False

eq xs ys = length xs == length ys && all isJust (zipWith wildcard xs ys)


matchOne :: Pat -> String -> Bool
matchOne (Stars x) y = isJust $ wildcard x y
matchOne Skip _ = False


matchBoolWith :: Pats -> FilePath -> Bool
matchBoolWith (Pats pat) = f . (\x -> if null x then [""] else x) . filter (/= ".") . split isPathSeparator
    where f = matchRepats $ repats $ Pats $ optimise pat


-- | Like '?==', but returns 'Nothing' on if there is no match, otherwise 'Just' with the list
--   of fragments matching each wildcard. For example:
--
-- @
-- 'filePattern' \"**\/*.c\" \"test.txt\" == Nothing
-- 'filePattern' \"**\/*.c\" \"foo.c\" == Just [\"",\"foo\"]
-- 'filePattern' \"**\/*.c\" \"bar\/baz\/foo.c\" == Just [\"bar\/baz/\",\"foo\"]
-- @
--
--   Note that the @**@ will often contain a trailing @\/@, and even on Windows any
--   @\\@ separators will be replaced by @\/@.
matchWith :: Pats -> FilePath -> Maybe [String]
matchWith (Pats ps) = listToMaybe . match ps . (\x -> if null x then [""] else x) . filter (/= ".") . split isPathSeparator


---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

specialsWith :: Pats -> [Int]
specialsWith = concatMap f . fromPats
    where
        f Skip = [0]
        f (Stars (Wildcard _ xs _)) = replicate (length xs + 1) 1
        f (Stars Literal{}) = []

-- | Is the pattern free from any * and //.
simpleWith :: Pats -> Bool
simpleWith = null . specialsWith

-- | Do they have the same * and // counts in the same order
compatibleWith :: [Pats] -> Bool
compatibleWith [] = True
compatibleWith (x:xs) = all ((==) (specialsWith x) . specialsWith) xs

-- | Given a successful 'match', substitute it back in to a 'compatible' pattern.
--
-- > p '?==' x ==> substitute (extract p x) p == x
substituteWith :: Partial => String -> [String] -> (FilePattern, Pats) -> FilePath
substituteWith func parts (pat, Pats ps)
    | let want = sum $ map count ps, let got = length parts, want /= got =
        error $ func ++ " given the wrong number of parts, wanted " ++
                show want ++ ", got " ++ show got ++
                ", for the pattern " ++ show pat ++ " and parts " ++ show parts
    | otherwise = intercalate "/" $ concat $ snd $ mapAccumL f parts ps
    where
        count Skip = 1
        count (Stars Literal{}) = 0
        count (Stars (Wildcard _ mid _)) = length mid + 1

        f ms (Stars (Literal x)) = (ms, [x])
        f (m:ms) Skip = (ms, splitSep m)
        f ms (Stars (Wildcard pre mid post)) = (ms2, [concat $ pre : zipWith (++) ms1 (mid++[post])])
            where (ms1,ms2) = splitAt (length mid + 1) ms
        f _ _ = error $ "substitution, internal error, with " ++ show parts ++ " " ++ show ps

        splitSep = linesBy (== '/')


---------------------------------------------------------------------
-- EFFICIENT PATH WALKING

-- | Given a list of files, return a list of things I can match in this directory
--   plus a list of subdirectories and walks that apply to them.
--   Use WalkTo when the list can be predicted in advance
data Walk = Walk ([String] -> ([String],[(String,Walk)]))
          | WalkTo            ([String],[(String,Walk)])

walkWith :: [Pats] -> (Bool, Walk)
walkWith patterns = (any (\p -> isEmpty p || not (null $ match p [""])) ps2, f ps2)
    where
        ps2 = map (optimise . fromPats) patterns

        f (nubOrd -> ps)
            | all isLit fin, all (isLit . fst) nxt = WalkTo (map fromLit fin, map (fromLit *** f) nxt)
            | otherwise = Walk $ \xs ->
                (if finStar then xs else filter (\x -> any (`matchOne` x) fin) xs
                ,[(x, f ys) | x <- xs, let ys = concat [b | (a,b) <- nxt, matchOne a x], not $ null ys])
            where
                finStar = star `elem` fin
                fin = nubOrd $ mapMaybe final ps
                nxt = groupSort $ concatMap next ps


next :: [Pat] -> [(Pat, [Pat])]
next (Skip:xs) = (star,Skip:xs) : next xs
next (x:xs) = [(x,xs) | not $ null xs]
next [] = []

final :: [Pat] -> Maybe Pat
final (Skip:xs) = if isEmpty xs then Just star else final xs
final (x:xs) = if isEmpty xs then Just x else Nothing
final [] = Nothing

isEmpty :: [Pat] -> Bool
isEmpty = all (== Skip)
