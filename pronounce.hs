module Main where

import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC 

import System

main :: IO ()
main = do
  args <- getArgs
--  when (length(args) != 2) do
--         putStr "Usage: pronounce <dictionary binary> <word to pronounce>"
--         return 
  pronunciations <- fmap Binary.decode (LB.readFile (args!!0)) :: IO (Map.Map B.ByteString B.ByteString)
  let pron = Map.lookup (BC.pack (args!!1)) pronunciations
  case pron of
    Nothing -> return ()
    Just p -> putStr ((BC.unpack p) ++ "\n")
  