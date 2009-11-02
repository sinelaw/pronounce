module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC 

import System

upper = map Char.toUpper
lower = map Char.toLower

replace c d [] = []
replace c d (x:xs) | c==x = d : (replace c d xs)
replace c d (x:xs) = x : (replace c d xs)

main :: IO ()
main = do
  args <- getArgs
--  when (length(args) != 2) do
--         putStr "Usage: pronounce <dictionary binary> <word to pronounce>"
--         return 
  pronunciations <- fmap Binary.decode (LB.readFile (args!!0)) :: IO (Map.Map B.ByteString B.ByteString)
  let pron x = (Map.lookup (BC.pack (upper x)) pronunciations)
      prons = map pron args
      printEm Nothing = []
      printEm (Just p) = (replace ' ' '-') . lower . BC.unpack $ p

  putStr . concat . (List.intersperse " ") $ (map printEm prons)
  putStr "\n"
