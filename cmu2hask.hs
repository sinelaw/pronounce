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
import qualified Data.PerfectHash as PH

-- instance Binary.Binary PerfectHash a where
--    put ph = 

main :: IO ()
main = do
  args <- System.getArgs
  let filename = args!!0
  result <- (LP.parseFromFile convLines filename) 
  case result of
    Left err       -> putStr $ "Parse Error: " ++ show err ++ "\n"
    Right pron_map -> do
                let res = PH.lookup pron_map $ packFromString (args!!1) --Binary.encodeFile "test.bin" pron_map
                case res of 
                  Nothing    -> return ()
                  Just pron  -> putStr pron
  return ()


type HashS = PH.PerfectHash String

-- todo why doesn't this keep running? just does one line...
convLines :: LP.Parser HashS
convLines = do converted <- P.many $ (P.try $ parseComment) P.<|> parseEntry
               return $ PH.fromList . concat $ converted
                 
         

unpackToString = LBC.unpack
packFromString = LBC.pack

parseComment :: LP.Parser [(B.ByteString, String)]
parseComment = do P.spaces
                  P.string ";;;"
                  P.many (P.noneOf "\n")
                  P.newline
                  return []

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseEntry ::  LP.Parser [(B.ByteString, String)]
parseEntry = do P.spaces
                named_char <- P.optionMaybe . P.many1 . P.oneOf $ "-,:;!?/.'\"()&#%{}"
                word <- P.many1  $ P.noneOf " "
                P.spaces
                pronunciation <- P.many $ P.noneOf "\n"
                P.newline
                return $ [(packFromString word, pronunciation)]
                


            