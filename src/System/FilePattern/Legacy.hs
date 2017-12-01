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
module System.FilePattern.Legacy
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
import Data.Maybe
import System.FilePattern.Internal
import System.FilePattern.Parser(parseLegacy, addUnsafeLegacyWarning)
import Prelude


---------------------------------------------------------------------
-- PATTERNS

-- | Used for internal testing
internalTest :: IO ()
internalTest = do
    let x # y = when (parseLegacy x /= y) $ fail $ show ("FilePattern.internalTest",x,parseLegacy x,y)
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
(?==) = matchWith parseLegacy


-- | Like 'FilePattern.filePattern' but also deals with @\/\/@ patterns.
filePattern :: FilePattern -> FilePath -> Maybe [String]
filePattern = filePatternWith parseLegacy

---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

-- | Like 'FilePattern.simple' but also deals with @\/\/@ patterns.
simple :: FilePattern -> Bool
simple = simpleWith parseLegacy

-- | Like 'FilePattern.compatible' but also deals with @\/\/@ patterns.
compatible :: [FilePattern] -> Bool
compatible = compatibleWith parseLegacy

-- | Like 'FilePattern.extract' but also deals with @\/\/@ patterns.
extract :: FilePattern -> FilePath -> [String]
extract = extractWith parseLegacy

-- | Like 'FilePattern.substitute' but also deals with @\/\/@ patterns.
substitute :: [String] -> FilePattern -> FilePath
substitute = substituteWith parseLegacy


---------------------------------------------------------------------
-- EFFICIENT PATH WALKING

-- | Like 'FilePattern.walk' but also deals with @\/\/@ patterns.
walk :: [FilePattern] -> (Bool, Walk)
walk = walkWith parseLegacy
