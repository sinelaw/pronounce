module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC 

import System.Environment(getArgs)

upper = map Char.toUpper
lower = map Char.toLower

replace c d = map (\x -> if x == c then d else x) 

main :: IO ()
main = do
  args <- getArgs
--  when (length(args) != 2) do
--         putStr "Usage: pronounce <dictionary binary> <word to pronounce>"
--         return 
  pronunciations <- fmap Binary.decode (LB.readFile (args!!0)) :: IO (Map.Map B.ByteString B.ByteString)
  let pron x = (x, Map.lookup (BC.pack . upper $ x) pronunciations)
      prons = map pron (tail args)
      printEm (x, Nothing) = x
      printEm (x, Just p)  = replace ' ' '-' . lower . BC.unpack $ p

  putStr . concat . List.intersperse " " $ map printEm prons
  putStr "\n"
