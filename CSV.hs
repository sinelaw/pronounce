{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
-- written by mmorrow @ #haskell from freenode.net

module CSV where


import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

import Data.ByteString (ByteString,pack)
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w,w2c)
import Data.ByteString.Unsafe
import Data.Word
import Data.List
import Data.Monoid(Monoid(..))

-----------------------------------------------------------------------------

type Csv = [Rec]
type Rec = [Field]
type Field = ByteString

parseCsv :: ByteString -> Csv
parseCsv s
  | B.null s = []
parseCsv s = let (r,s') = go Start [] [] s in r : parseCsv s'

sep :: Word8
sep = c2w ','
quote :: Word8
quote = c2w '"'
nl :: Word8
nl = c2w '\n'


fieldSep = pack . fmap c2w $ "\",\""

q1 = pack [c2w '"']

test0 = return . parseCsv . B.filter (/=c2w '\r') =<< B.readFile "ah.csv"


ppCsv :: Csv -> L.ByteString
ppCsv = L.fromChunks . fmap ppRecLn


ppRecLn :: Rec -> ByteString
ppRecLn = mconcat . (++[pack[nl]]) . wrap q1 q1 fieldSep

ppRec :: Rec -> ByteString
ppRec = mconcat . wrap q1 q1 fieldSep

wrap :: a -> a -> a -> [a] -> [a]
wrap _ _ _ [] = []
wrap l r sep xs = l : go r sep xs
  where go _ _   []     = []
        go r _   [x]    = [x, r]
        go r sep (x:xs) = x : sep : go r sep xs


ppCsvStr :: [[String]] -> String
ppCsvStr = unlines . fmap showRec
  where wrap :: a -> a -> [a] -> [a]
        wrap l r xs = l : xs ++ [r]
        replace :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
        replace _ _ [] = []
        replace p f (a:as)
          | p a        = f a ++ replace p f as
          | otherwise  = a : replace p f as
        showRec :: [String] -> String
        showRec = intercalate "," . fmap showField
        showField :: String -> String
        showField = wrap '"' '"'
          . replace (=='"') (const ['"','"'])




-----------------------------------------------------------------------------

data Mode
  = Start
  | Field
  | QField
  | QLook
  deriving (Eq,Ord,Show,Read)

-- add error reporting
go :: Mode -> [Field] -> [Word8] -> ByteString -> (Rec,ByteString)
go _ fs acc cs
  | B.null cs = (unwind fs acc, B.empty)  -- could be an error
go Start fs _ cs =
  case unsafeHead cs of
    c | c == sep    -> go Start (B.empty:fs) [] (unsafeTail cs)
      | c == quote  -> go QField fs [] (unsafeTail cs)
      | c == nl     -> (unwind (B.empty:fs) [], unsafeTail cs)
      | otherwise   -> go Field fs [c] (unsafeTail cs)
go Field fs acc cs =
  case unsafeHead cs of
    c | c == sep    -> go Start (pack (reverse acc):fs) [] (unsafeTail cs)
      | c == nl     -> (unwind fs acc, unsafeTail cs)
      | otherwise   -> go Field fs (c:acc) (unsafeTail cs)  -- we're taking quote here
go QField fs acc cs =
  case unsafeHead cs of
    c | c == quote  -> go QLook fs acc (unsafeTail cs)
      | otherwise   -> go QField fs (c:acc) (unsafeTail cs)
go QLook fs acc cs =
  case unsafeHead cs of
    c | c == quote  -> go QField fs (quote:acc) (unsafeTail cs)
      | c == sep    -> go Start (pack (reverse acc):fs) [] (unsafeTail cs)
      | c == nl     -> (unwind fs acc,unsafeTail cs)
      | otherwise   -> go Start (pack (reverse acc):fs) []   -- errorish
                        (B.dropWhile (not . (`elem` [sep,nl])) (unsafeTail cs))

unwind :: [Field] -> [Word8] -> [Field]
unwind xss xs = reverse (pack (reverse xs) : xss)

-----------------------------------------------------------------------------


data PState = PS Mode [Field] [Word8]
  deriving (Eq,Ord,Show,Read)

startPState :: PState
startPState = PS Start [] []


unwindPState :: PState -> Rec
unwindPState (PS _ fs acc) = unwind fs acc



chunked :: PState -> ByteString -> Either PState (Rec, ByteString)
chunked st cs
  | B.null cs = Left st
chunked (PS Start fs _) cs =
  case unsafeHead cs of
    c | c == sep    -> chunked (PS Start (B.empty:fs) []) (unsafeTail cs)
      | c == quote  -> chunked (PS QField fs []) (unsafeTail cs)
      | c == nl     -> Right (unwindPState $ PS Start (B.empty:fs) [], unsafeTail cs)
      | otherwise   -> chunked (PS Field fs [c]) (unsafeTail cs)
chunked st@(PS Field fs acc) cs =
  case unsafeHead cs of
    c | c == sep    -> chunked (PS Start (pack (reverse acc):fs) []) (unsafeTail cs)
      | c == nl     -> Right (unwindPState $ st, unsafeTail cs)
      | otherwise   -> chunked (PS Field fs (c:acc)) (unsafeTail cs)
chunked st@(PS QField fs acc) cs =
  case unsafeHead cs of
    c | c == quote  -> chunked (PS QLook fs acc) (unsafeTail cs)
      | otherwise   -> chunked (PS QField fs (c:acc)) (unsafeTail cs)
chunked st@(PS QLook fs acc) cs =
  case unsafeHead cs of
    c | c == quote  -> chunked (PS QField fs (quote:acc)) (unsafeTail cs)
      | c == sep    -> chunked (PS Start (pack (reverse acc):fs) []) (unsafeTail cs)
      | c == nl     -> Right (unwindPState $ st, unsafeTail cs)
      | otherwise   -> chunked (PS Start (pack (reverse acc):fs) [])
                        (B.dropWhile (not . (`elem` [sep,nl])) (unsafeTail cs))



parseChunk :: PState -> ByteString -> (Csv, PState)
parseChunk = go id
  where go k st s = case chunked st s of
                      Left st' -> (k [], st')
                      Right (r, s') -> go (k . (r:)) startPState s'



chunkedCsv :: [ByteString] -> Csv
chunkedCsv = go startPState
  where go _  []      = []
        go st [x]     = let (csv, st') = parseChunk st x
                            end = unwindPState st'
                        in csv ++ case end of
                                    [x] | B.null x -> []
                                    _  -> [end]
        go st (x:xs)  = let (csv, st') = parseChunk st x
                        in csv ++ go st' xs



lazyCsv :: L.ByteString -> Csv
lazyCsv = chunkedCsv . L.foldrChunks (:) []








