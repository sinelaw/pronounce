module Main where

import qualified ConvertedCMUDict
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  let pron = Map.lookup args!!0 pronunciations
  case pron of
    Nothing -> return ()
    Just p -> putStr p
  