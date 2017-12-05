{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.FilePattern.Legacy(main) where

import Control.Exception
import Control.Monad
import Data.List.Extra
import System.FilePattern.Legacy
import System.FilePattern.Type
import System.FilePattern.Parser(parseLegacy)
import System.FilePath(isPathSeparator)
import System.Info.Extra
import Data.IORef
import System.IO.Unsafe
import Test.QuickCheck hiding ((===))

assertBool :: Bool -> String -> IO ()
assertBool b msg = unless b $ error $ "ASSERTION FAILED: " ++ msg

infix 4 ===

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = assertBool (a == b) $ "failed in ===\nLHS: " ++ show a ++ "\nRHS: " ++ show b

newtype Pattern = Pattern FilePattern deriving (Show,Eq)
newtype Path    = Path    FilePath    deriving (Show,Eq)

-- Since / and * are the only "interesting" elements, just add ab to round out the set

instance Arbitrary Pattern where
    arbitrary = fmap Pattern $ listOf $ elements "\\/*ab"
    shrink (Pattern x) = map Pattern $ shrinkList (\x -> ['/' | x == '\\']) x

instance Arbitrary Path where
    arbitrary = fmap Path $ listOf $ elements "\\/ab"
    shrink (Path x) = map Path $ shrinkList (\x -> ['/' | x == '\\']) x

-- | Used for internal testing
internalTest :: IO ()
internalTest = do
    let x # y = when (parseLegacy x /= Pats x y) $ fail $ show ("FilePattern.internalTest",x,parseLegacy x,y)
    "" # [Lit ""]
    "x" # [Lit "x"]
    "/" # [Lit "",Lit ""]
    "x/" # [Lit "x",Lit ""]
    "/x" # [Lit "",Lit "x"]
    "x/y" # [Lit "x",Lit "y"]
    "//" # [Skip]
    "**" # [Skip]
    "//x" # [Skip, Lit "x"]
    "**/x" # [Skip, Lit "x"]
    "x//" # [Lit "x", Skip]
    "x/**" # [Lit "x", Skip]
    "x//y" # [Lit "x",Skip, Lit "y"]
    "x/**/y" # [Lit "x",Skip, Lit "y"]
    "///" # [Skip1, Lit ""]
    "**/**" # [Skip,Skip]
    "**/**/" # [Skip, Skip, Lit ""]
    "///x" # [Skip1, Lit "x"]
    "**/x" # [Skip, Lit "x"]
    "x///" # [Lit "x", Skip, Lit ""]
    "x/**/" # [Lit "x", Skip, Lit ""]
    "x///y" # [Lit "x",Skip, Lit "y"]
    "x/**/y" # [Lit "x",Skip, Lit "y"]
    "////" # [Skip, Skip]
    "**/**/**" # [Skip, Skip, Skip]
    "////x" # [Skip, Skip, Lit "x"]
    "x////" # [Lit "x", Skip, Skip]
    "x////y" # [Lit "x",Skip, Skip, Lit "y"]
    "**//x" # [Skip, Skip, Lit "x"]


main = do
    legacyTest
    internalTest
    let norm = filter (/= ".") . split isPathSeparator

    let f b pat file = do
            assertBool (b == (pat ?== file)) $ show pat ++ " ?== " ++ show file ++ "\nEXPECTED: " ++ show b
            assertBool (b == walker walk pat file) $ show pat ++ " `walker` " ++ show file ++ "\nEXPECTED: " ++ show b
            when b $ assertBool (norm (substitute (extract pat file) pat) == norm file) $
                "FAILED substitute/extract property\nPattern: " ++ show pat ++ "\nFile: " ++ show file ++ "\n" ++
                "Extracted: " ++ show (extract pat file) ++ "\nSubstitute: " ++ show (substitute (extract pat file) pat)

    f True "//*.c" "foo/bar/baz.c"
    f True "**/*.c" "foo/bar/baz.c"
    --f True (toNative "//*.c") "foo/bar\\baz.c"
    --f True (toNative "**/*.c") "foo/bar\\baz.c"
    f True "*.c" "baz.c"
    f True "//*.c" "baz.c"
    f True "**/*.c" "baz.c"
    f True "test.c" "test.c"
    f False "*.c" "foor/bar.c"
    f False "*/*.c" "foo/bar/baz.c"
    f False "foo//bar" "foobar"
    f False "foo/**/bar" "foobar"
    f False "foo//bar" "foobar/bar"
    f False "foo/**/bar" "foobar/bar"
    f False "foo//bar" "foo/foobar"
    f False "foo/**/bar" "foo/foobar"
    f True "foo//bar" "foo/bar"
    f True "foo/**/bar" "foo/bar"
    --f True "foo/bar" (toNative "foo/bar")
    --f True (toNative "foo/bar") "foo/bar"
    --f True (toNative "foo/bar") (toNative "foo/bar")
    f True "//*" "/bar"
    f True "**/*" "/bar"
    f True "/bob//foo" "/bob/this/test/foo"
    f True "/bob/**/foo" "/bob/this/test/foo"
    f False "/bob//foo" "bob/this/test/foo"
    f False "/bob/**/foo" "bob/this/test/foo"
    f True "bob//foo/" "bob/this/test/foo/"
    f True "bob/**/foo/" "bob/this/test/foo/"
    f False "bob//foo/" "bob/this/test/foo"
    f False "bob/**/foo/" "bob/this/test/foo"
    f True "a//" "a"
    f True "a/**" "a"
    f True "/a//" "/a"
    f True "/a/**" "/a"
    f True "///a//" "/a"
    f True "**/a/**" "/a"
    f False "///" ""
    f True "///" "/"
    f True "/**" "/"
    f True "///" "a/"
    f True "**/" "a/"
    f True "////" ""
    f True "**/**" ""
    f True "x///y" "x/y"
    f True "x/**/y" "x/y"
    f True "x///" "x/"
    f True "x/**/" "x/"
    f True "x///" "x/foo/"
    f True "x/**/" "x/foo/"
    f False "x///" "x"
    f False "x/**/" "x"
    f True "x///" "x/foo/bar/"
    f True "x/**/" "x/foo/bar/"
    f False "x///" "x/foo/bar"
    f False "x/**/" "x/foo/bar"
    f True "x///y" "x/z/y"
    f True "x/**/*/y" "x/z/y"
    f True "" ""
    f False "" "y"
    f False "" "/"

    f True "*/*" "x/y"
    f False "*/*" "x"
    f True "//*" "x"
    f True "**/*" "x"
    f True "//*" ""
    f True "**/*" ""
    f True "*//" "x"
    f True "*/**" "x"
    f True "*//" ""
    f True "*/**" ""
    f True "*//*" "x/y"
    f True "*/**/*" "x/y"
    f False "*//*" ""
    f False "*/**/*" ""
    f False "*//*" "x"
    f False "*/**/*" "x"
    f False "*//*//*" "x/y"
    f False "*/**/*/**/*" "x/y"
    f True "//*/" "/"
    f True "**/*/" "/"
    f True "*/////" "/"
    f True "*/**/**/" "/"
    f False "b*b*b*//" "bb"
    f False "b*b*b*/**" "bb"

    f True "**" "/"
    f True "**/x" "/x"
    f True "**" "x/"
    f True "**" "\\\\drive"
    f True "**" "C:\\drive"
    f True "**" "C:drive"

    -- We support ignoring '.' values in FilePath as they are inserted by @filepath@ a lot
    f True "./file" "file"
    -- f True "/file" "file"
    f True "foo/./bar" "foo/bar"
    f True "foo/./bar" "foo/./bar"
    f False "foo/./bar" "foo/bob"

    filePattern "**/*.c" "test.txt" === Nothing
    filePattern "**/*.c" "foo.c" === Just ["","foo"]
    filePattern "**/*.c" "bar/baz/foo.c" === Just ["bar/baz/","foo"]
    filePattern "**/*.c" "bar\\baz\\foo.c" === Just
        (if isWindows then ["bar/baz/","foo"] else ["","bar\\baz\\foo"])

    simple "a*b" === False
    simple "a//b" === False
    simple "a/**/b" === False
    simple "/a/b/cccc_" === True
    simple "a///b" === False
    simple "a/**/b" === False

    assertBool (compatible []) "compatible"
    assertBool (compatible ["//*a.txt","foo//a*.txt"]) "compatible"
    assertBool (compatible ["**/*a.txt","foo/**/a*.txt"]) "compatible"
    assertBool (compatible ["//*a.txt","foo/**/a*.txt"]) "compatible"
    assertBool (not $ compatible ["//*a.txt","foo//a*.*txt"]) "compatible"
    assertBool (not $ compatible ["**/*a.txt","foo/**/a*.*txt"]) "compatible"
    extract "//*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "**/*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "//*a.txt" "testa.txt" === ["","test"]
    extract "**/*a.txt" "testa.txt" === ["","test"]
    extract "//a.txt" "a.txt" === [""]
    extract "**/a.txt" "a.txt" === [""]
    extract "//a.txt" "/a.txt" === ["/"]
    extract "a//b" "a/b" === [""]
    extract "a/**/b" "a/b" === [""]
    extract "a//b" "a/x/b" === ["x/"]
    extract "a/**/b" "a/x/b" === ["x/"]
    extract "a//b" "a/x/y/b" === ["x/y/"]
    extract "a/**/b" "a/x/y/b" === ["x/y/"]
    extract "a///b" "a/x/y/b" === ["x/y/"]
    extract "a/**/**/b" "a/x/y/b" === ["","x/y/"]
    extract "//*a*.txt" "testada.txt" === ["","test","da"]
    extract "**/*a*.txt" "testada.txt" === ["","test","da"]
    --extract (toNative "//*a*.txt") "testada.txt" === ["","test","da"]
    --extract (toNative "**/*a*.txt") "testada.txt" === ["","test","da"]
    substitute ["","test","da"] "//*a*.txt" === "testada.txt"
    substitute ["","test","da"] "**/*a*.txt" === "testada.txt"
    substitute  ["foo/bar/","test"] "//*a.txt" === "foo/bar/testa.txt"
    substitute  ["foo/bar/","test"] "**/*a.txt" === "foo/bar/testa.txt"

    (False, Walk _) <- return $ walk ["*.xml"]
    (False, Walk _) <- return $ walk ["//*.xml"]
    (False, Walk _) <- return $ walk ["**/*.xml"]
    (False, WalkTo ([], [("foo",Walk _)])) <- return $ walk ["foo//*.xml"]
    (False, WalkTo ([], [("foo",Walk _)])) <- return $ walk ["foo/**/*.xml"]
    (False, WalkTo ([], [("foo",WalkTo ([],[("bar",Walk _)]))])) <- return $ walk ["foo/bar/*.xml"]
    (False, WalkTo (["a"],[("b",WalkTo (["c"],[]))])) <- return $ walk ["a","b/c"]
    ([], [("foo",WalkTo ([],[("bar",Walk _)]))]) <- let (False, Walk f) = walk ["*/bar/*.xml"] in return $ f ["foo"]
    (False, WalkTo ([],[("bar",Walk _),("baz",Walk _)])) <- return $ walk ["bar/*.xml","baz//*.c"]
    (False, WalkTo ([],[("bar",Walk _),("baz",Walk _)])) <- return $ walk ["bar/*.xml","baz/**/*.c"]
    (False, WalkTo ([], [])) <- return $ walk []
    (True, Walk _) <- return $ walk ["//"]
    (True, Walk _) <- return $ walk ["**"]
    (True, WalkTo _) <- return $ walk [""]

    Success{} <- quickCheckWithResult stdArgs{maxSuccess=1000} $ \(Pattern p) (Path x) ->
        let label _ = property in
            -- Ignore label to workaround QuickCheck space-leak
            -- See #450 and https://github.com/nick8325/quickcheck/pull/93
        let b = p ?== x in (if b then property else label "No match") $ unsafePerformIO $ do f b p x; return True
    return ()


legacyTest :: IO ()
legacyTest = do
    ref <- newIORef Nothing
    addUnsafeLegacyWarning $ \x -> modifyIORef ref $ fmap (++ [x])

    let f b x = do
            writeIORef ref $ Just []
            evaluate $ compatible [x,x]
            res <- readIORef ref
            writeIORef ref Nothing -- to avoid a memory leak
            res === Just (if b then [x,x] else [])

    f False "foo/**/x"
    f True  "foo/x//y"
    f True  "foo//x//y"
    f False "foo/**/x/*.foo"


walker :: ([FilePattern] -> (Bool, Walk)) -> FilePattern -> FilePath -> Bool
walker walking a b = f (split isPathSeparator b) $ snd $ walking [a]
    where
        f (".":xs) w = f xs w
        f (x:xs) (Walk op) = f (x:xs) $ WalkTo $ op [x]
        f [x]    (WalkTo (file, dir)) = x `elem` file
        f (x:xs) (WalkTo (file, dir)) | Just w <- lookup x dir = f xs w
        f _ _ = False
