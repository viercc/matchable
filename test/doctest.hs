module Main(main) where

import System.FilePath.Glob as Glob
import Test.DocTest

sourceDir :: FilePath
sourceDir = "src"

sourcePatterns :: [Pattern]
sourcePatterns = Glob.compile <$> [ "**/*.hs", "**/*.lhs" ]

main :: IO ()
main = do
  globResult <- globDir sourcePatterns sourceDir
  let sourceFiles = [ f | f <- concat globResult ]
  doctest $ ("-i" ++ sourceDir) : sourceFiles

