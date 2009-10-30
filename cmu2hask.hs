--- options to solve speed problem:
--- * use CSV.hs which is by mmorow and very fast
--- * use PerfectHash and Binary marshalling

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Text.Parsec.ByteString.Lazy as LP
import qualified Text.Parsec as P

import qualified System

import qualified Data.Char
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- System.getArgs
  let filename = args!!0
  result <- (LP.parseFromFile (convLines Map.empty) filename) 
  case result of
    Left err       -> putStr $ "Parse Error: " ++ show err ++ "\n"
    Right pron_map -> case Map.lookup (args!!1) pron_map of 
                        Nothing -> return ()
                        Just pron -> putStr $ pron ++ "\n"
  return ()


type MapSS = Map.Map String String

-- todo why doesn't this keep running? just does one line...
convLines :: MapSS -> LP.Parser MapSS
convLines m = do converted <- P.many $ (P.try $ parseComment m) P.<|> (parseEntry m)
                 return $ Map.unions converted
                 
         

unpackToString = map (Data.Char.chr . fromIntegral) . LB.unpack
packFromString = LB.pack . (map $ fromIntegral . Data.Char.ord)

parseComment :: MapSS -> LP.Parser MapSS
parseComment m = do P.spaces
                    P.string ";;;"
                    P.many (P.noneOf "\n")
                    P.newline
                    return m

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseEntry :: MapSS -> LP.Parser MapSS
parseEntry m = do P.spaces
                  named_char <- P.optionMaybe . P.many1 . P.oneOf $ "-,:;!?/.'\"()&#%{}"
                  word <- P.many1  $ P.noneOf " "
                  P.spaces
                  pronunciation <- P.many $ P.noneOf "\n"
                  P.newline
                  return $ (Map.insert word pronunciation m)
                


            