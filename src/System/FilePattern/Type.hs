{-# LANGUAGE PatternGuards #-}

-- | The types involved
module System.FilePattern.Type(
    FilePattern,
    Pats(..),
    Pat(..),
    isLit, fromLit
    ) where


-- | A type synonym for file patterns, containing @**@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
--
--   Most 'FilePath' values lacking @.@ and @..@ are suitable as 'FilePattern' values which match
--   only that specific file. On (Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators '<.>' and '</>'.
type FilePattern = String


-- | Combination of the original FilePattern (for error messages),
--   along with the parsed representation.
data Pats = Pats FilePattern [Pat]
    deriving (Eq,Show)

data Pat = Lit String -- ^ foo
         | Star   -- ^ /*/
         | Skip -- ^ //
         | Skip1 -- ^ //, but must be at least 1 element
         | Stars String [String] String -- ^ *foo*, prefix (fixed), infix floaters, suffix
                                        -- e.g. *foo*bar = Stars "" ["foo"] "bar"
            deriving (Show,Eq,Ord)


isLit :: Pat -> Bool
isLit Lit{} = True
isLit _ = False

fromLit :: Pat -> String
fromLit (Lit x) = x
fromLit _ = error "fromLit: applied to non Lit"
