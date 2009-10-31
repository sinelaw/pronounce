module Main where

import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LB

import System

main :: IO ()
main = do
  args <- getArgs
  pronunciations <- fmap Binary.decode (LB.readFile (args!!0)) :: IO (Map.Map String String)
  let pron = Map.lookup (args!!1) pronunciations
  case pron of
    Nothing -> return ()
    Just p -> putStr (p ++ "\n")
  