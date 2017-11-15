{-# LANGUAGE PatternGuards #-}

module FilePattern.Legacy(
    -- * Primitive API, as exposed
    FilePattern, (?==), (<//>),
    -- * General API, used by other people.
    filePattern,
    -- * Optimisation opportunities
    simple,
    -- * Multipattern file rules
    compatible, extract, substitute,
    -- * Accelerated searching
    Walk(..), walk,
    -- * Testing only
    internalTest, isRelativePath, isRelativePattern
    ) where

import Control.Monad
import Data.List.Extra
import Data.Maybe
import FilePattern.Internal
import Prelude
import System.FilePath (isPathSeparator)

infixr 5 <//>

-- | Join two 'FilePattern' values by inserting two @\/@ characters between them.
--   Will first remove any trailing path separators on the first argument, and any leading
--   separators on the second.
--
-- > "dir" <//> "*" == "dir//*"
(<//>) :: FilePattern -> FilePattern -> FilePattern
a <//> b = dropWhileEnd isPathSeparator a ++ "//" ++ dropWhile isPathSeparator b


---------------------------------------------------------------------
-- PATTERNS


lexer :: FilePattern -> [Lexeme]
lexer "" = []
lexer (x1:x2:xs) | isPathSeparator x1, isPathSeparator x2 = SlashSlash : lexer xs
lexer (x1:xs) | isPathSeparator x1 = Slash : lexer xs
lexer xs = Str a : lexer b
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
filePattern :: FilePattern -> FilePath -> Maybe [String]
filePattern = filePatternWith parse

---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

-- | Is the pattern free from any * and //.
simple :: FilePattern -> Bool
simple = simpleWith parse

-- | Do they have the same * and // counts in the same order
compatible :: [FilePattern] -> Bool
compatible = compatibleWith parse

-- | Extract the items that match the wildcards. The pair must match with '?=='.
extract :: FilePattern -> FilePath -> [String]
extract = extractWith parse

-- | Given the result of 'extract', substitute it back in to a 'compatible' pattern.
--
-- > p '?==' x ==> substitute (extract p x) p == x
substitute :: [String] -> FilePattern -> FilePath
substitute = substituteWith parse


---------------------------------------------------------------------
-- EFFICIENT PATH WALKING

-- | Efficient path walking with a pattern
walk :: [FilePattern] -> (Bool, Walk)
walk = walkWith parse
