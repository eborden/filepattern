{-# LANGUAGE PatternGuards #-}

-- | A module for pattern matching on file names.
--
-- >>> "//*.png" ?== "/foo/bar/baz.png"
-- True
--
--   This module supports @*@ and @**@ like "FilePattern", and also supports @\/\/@.
--   The inclusion of @//@ in patterns was a misfeature, as it interacts poorly with
--   'Development.Shake.FilePath.<.>' and 'Development.Shake.FilePath.</>'.
--   This module will be deleted at some point in the future.
module FilePattern.Legacy
    {-# DEPRECATED "Use module FilePattern and avoid // in the patterns" #-}
    (
    -- * Primitive API
    FilePattern, (?==),
    -- * General API
    filePattern,
    -- * Optimisation opportunities
    simple,
    -- * Multipattern file rules
    compatible, extract, substitute,
    -- * Accelerated searching
    Walk(..), walk,
    -- * Deprecation path
    addUnsafeLegacyWarning,
    -- * Testing only
    internalTest, isRelativePath, isRelativePattern
    ) where

import Control.Monad
import Data.List.Extra
import Data.Maybe
import Data.IORef
import System.IO.Unsafe
import FilePattern.Internal
import Prelude
import System.FilePath (isPathSeparator)


---------------------------------------------------------------------
-- LEGACY WARNING

{-# NOINLINE legacyWarning #-}
legacyWarning :: IORef (FilePattern -> IO ())
legacyWarning = unsafePerformIO $ newIORef $ const $ return ()

-- | Add a callback that is run on every pattern containing @\/\/@.
--   Note that the callback will be run via 'unsafePerformIO'.
addUnsafeLegacyWarning :: (FilePattern -> IO ()) -> IO ()
addUnsafeLegacyWarning act = atomicModifyIORef legacyWarning $ \old -> (old >> act, ())

{-# NOINLINE traceLegacyWarning #-}
traceLegacyWarning :: FilePattern -> a -> a
traceLegacyWarning pat x = unsafePerformIO $ do
    warn <- readIORef legacyWarning
    warn pat
    return x


---------------------------------------------------------------------
-- PATTERNS

lexer :: FilePattern -> [Lexeme]
lexer x = f x x
    where
        -- first argument is the original full pattern, or "" if we've already warned
        f o "" = []
        f o (x1:x2:xs) | isPathSeparator x1, isPathSeparator x2 =
            (if o == "" then id else traceLegacyWarning o) $ SlashSlash : f "" xs
        f o (x1:xs) | isPathSeparator x1 = Slash : f o xs
        f o xs = Str a : f o b
            where (a,b) = break isPathSeparator xs


parse :: FilePattern -> [Pat]
parse = parseWith lexer


-- | Used for internal testing
internalTest :: IO ()
internalTest = do
    let x # y = when (parse x /= y) $ fail $ show ("FilePattern.internalTest",x,parse x,y)
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


-- | Match a 'FilePattern' against a 'FilePath', There are three special forms:
--
-- * @*@ matches an entire path component, excluding any separators.
--
-- * @\/\/@ matches an arbitrary number of path components, including absolute path
--   prefixes.
--
-- * @**@ as a path component matches an arbitrary number of path components, but not
--   absolute path prefixes.
--   Currently considered experimental.
--
--   Some examples:
--
-- * @test.c@ matches @test.c@ and nothing else.
--
-- * @*.c@ matches all @.c@ files in the current directory, so @file.c@ matches,
--   but @file.h@ and @dir\/file.c@ don't.
--
-- * @\/\/*.c@ matches all @.c@ files anywhere on the filesystem,
--   so @file.c@, @dir\/file.c@, @dir1\/dir2\/file.c@ and @\/path\/to\/file.c@ all match,
--   but @file.h@ and @dir\/file.h@ don't.
--
-- * @dir\/*\/*@ matches all files one level below @dir@, so @dir\/one\/file.c@ and
--   @dir\/two\/file.h@ match, but @file.c@, @one\/dir\/file.c@, @dir\/file.h@
--   and @dir\/one\/two\/file.c@ don't.
--
--   Patterns with constructs such as @foo\/..\/bar@ will never match
--   normalised 'FilePath' values, so are unlikely to be correct.
(?==) :: FilePattern -> FilePath -> Bool
(?==) = matchWith parse


-- | Like 'FilePattern.filePattern' but also deals with @\/\/@ patterns.
filePattern :: FilePattern -> FilePath -> Maybe [String]
filePattern = filePatternWith parse

---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

-- | Like 'FilePattern.simple' but also deals with @\/\/@ patterns.
simple :: FilePattern -> Bool
simple = simpleWith parse

-- | Like 'FilePattern.compatible' but also deals with @\/\/@ patterns.
compatible :: [FilePattern] -> Bool
compatible = compatibleWith parse

-- | Like 'FilePattern.extract' but also deals with @\/\/@ patterns.
extract :: FilePattern -> FilePath -> [String]
extract = extractWith parse

-- | Like 'FilePattern.substitute' but also deals with @\/\/@ patterns.
substitute :: [String] -> FilePattern -> FilePath
substitute = substituteWith parse


---------------------------------------------------------------------
-- EFFICIENT PATH WALKING

-- | Like 'FilePattern.walk' but also deals with @\/\/@ patterns.
walk :: [FilePattern] -> (Bool, Walk)
walk = walkWith parse
