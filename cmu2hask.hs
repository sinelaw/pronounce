--- options to solve speed problem:
--- * use CSV.hs which is by mmorow and very fast
--- * use PerfectHash and Binary marshalling

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as LBC 

import qualified Text.Parsec.ByteString.Lazy as LP
import qualified Text.Parsec as P

import qualified System

import qualified Data.Char
import qualified Data.Binary as Binary
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- System.getArgs
  let filename = args!!0
  result <- (LP.parseFromFile convLines filename) 
  case result of
    Left err       -> putStr $ "Parse Error: " ++ show err ++ "\n"
    Right pron_map -> Binary.encodeFile ((args!!0) ++ ".bin") pron_map
  return ()


type MapSS = Map.Map String String

-- todo why doesn't this keep running? just does one line...
convLines :: LP.Parser MapSS
convLines = do converted <- P.many $ (P.try $ parseComment) P.<|> parseEntry
               return $ Map.fromList . concat $ converted
                 
         

unpackToString = LBC.unpack
packFromString = LBC.pack

parseComment :: LP.Parser [(String, String)]
parseComment = do P.spaces
                  P.string ";;;"
                  P.many (P.noneOf "\n")
                  P.newline
                  return []

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseEntry ::  LP.Parser [(String, String)]
parseEntry = do P.spaces
                named_char <- P.optionMaybe . P.many1 . P.oneOf $ "-,:;!?/.'\"()&#%{}"
                word <- P.many1  $ P.noneOf " "
                P.spaces
                pronunciation <- P.many $ P.noneOf "\n"
                P.newline
                return $ [(word, pronunciation)]
                


            