{-# LANGUAGE PatternGuards #-}

-- | The types involved
module System.FilePattern.Type(
    FilePattern,
    Pats(..),
    Pat(..),
    Wildcard(..),
    wildcard,
    isLit, fromLit
    ) where

import Data.List.Extra

-- | A type synonym for file patterns, containing @**@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
--
--   Most 'FilePath' values lacking @.@ and @..@ are suitable as 'FilePattern' values which match
--   only that specific file. On (Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators '<.>' and '</>'.
type FilePattern = String


-- | Parsed 'FilePattern'.
newtype Pats = Pats {fromPats :: [Pat]}
    deriving (Eq,Show)

data Wildcard a = Wildcard a [a] a
    deriving (Show,Eq,Ord)

-- Only return the first (all patterns left-most) valid star matching
wildcard :: Eq a => Wildcard [a] -> [a] -> Maybe [[a]]
wildcard (Wildcard pre mid post) x = do
    y <- stripPrefix pre x
    z <- if null post then Just y else stripSuffix post y
    stripInfixes mid z
    where
        stripInfixes [] y = Just [y]
        stripInfixes (m:ms) y = do
            (a,z) <- stripInfix m y
            (a:) <$> stripInfixes ms z


data Pat = Lit String -- ^ foo
         | Star   -- ^ /*/
         | Skip -- ^ //
         | Skip1 -- ^ //, but must be at least 1 element
         | Stars (Wildcard String) -- ^ *foo*, prefix (fixed), infix floaters, suffix
                          -- e.g. *foo*bar = Stars "" ["foo"] "bar"
            deriving (Show,Eq,Ord)


isLit :: Pat -> Bool
isLit Lit{} = True
isLit _ = False

fromLit :: Pat -> String
fromLit (Lit x) = x
fromLit _ = error "fromLit: applied to non Lit"
