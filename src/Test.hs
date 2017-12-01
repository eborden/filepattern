module Main (main) where

import qualified Test.FilePattern as Current
import qualified Test.FilePattern.Legacy as Legacy

main = do
  Current.main
  Legacy.main
