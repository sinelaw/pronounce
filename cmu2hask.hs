
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Text.Parsec.ByteString.Lazy as LP
import qualified Text.Parsec as P

import qualified System

import qualified Data.Char

main :: IO ()
main = do
  args <- System.getArgs
  let filename = args!!0
  putStr "module ConvertedCMUDict where\n"
  putStr "import qualified Data.Map\n"
  putStr $ " -- Parsing " ++ show filename ++ "\n"
  putStr "pronunciations = Data.Map.fromList ["
  result <- (LP.parseFromFile convLines filename) 
  case result of
    Left err -> putStr $ "Parse Error: " ++ show err ++ "\n"
    Right bs -> putStr (unpackToString bs)
  putStr "]\n"
  return ()


-- todo why doesn't this keep running? just does one line...
convLines :: LP.Parser LB.ByteString
convLines = do converted <- P.many $ (P.try parseComment) P.<|> parseEntry
               return $ LB.concat converted
         
test :: LP.Parser LB.ByteString   
test = do a <- P.char 'A'
          return $ packFromString "bla"

unpackToString = map (Data.Char.chr . fromIntegral) . LB.unpack
packFromString = LB.pack . (map $ fromIntegral . Data.Char.ord)

parseComment :: LP.Parser LB.ByteString
parseComment = do P.spaces
                  P.string ";;;"
                  rest <- P.many (P.noneOf "\n")
                  P.newline
                  return $ packFromString $ " -- " ++ rest ++ "\n"

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseEntry :: LP.Parser LB.ByteString
parseEntry = do P.spaces
                named_char <- P.optionMaybe . P.many1 . P.oneOf $ "-,:;!?/.'\"()&#%{}"
                word <- P.many1  $ P.noneOf " "
                P.spaces
                pronunciation <- P.many $ P.noneOf "\n"
                P.newline
                return $ packFromString $ "   " ++ show (word, pronunciation) ++ ",\n"
                


            