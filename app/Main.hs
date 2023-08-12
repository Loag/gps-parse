module Main (main) where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  contents <- readFile path

  let linesInFile = lines contents
  
  mapM_ putStrLn (parseFile linesInFile)

parseSingle :: String -> String
parseSingle x = 
  maybe "empty" getName (parse x)

parseFile :: [String] -> [String]
parseFile = map parseSingle