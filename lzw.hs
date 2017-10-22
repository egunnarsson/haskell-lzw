module Codec.Compression.LZW ( compress, decompress ) where
--module Codec.Compression.LZW.Internal () where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.List
import Data.Word
import Data.Bits
import Data.Tuple
import Data.Maybe

type Chunk = B.ByteString
type Key = Word64

data Table a b = Table (M.Map a b) Key
type EncodeTable = Table Chunk Key
type DecodeTable = Table Key Chunk

--------------------------------------------------------------------------------
-- Util ------------------------------------------------------------------------
--------------------------------------------------------------------------------

lastToMaybe :: [a] -> Maybe a
lastToMaybe [] = Nothing
lastToMaybe x  = Just (last x)

takeBytes :: Int -> Word64 -> [Word8]
takeBytes 0 _ = []
takeBytes x w = fromIntegral (shiftR w (8 * (x - 1)) .&. 0xFF) : takeBytes (x - 1) w

putBytes :: [Word8] -> Word64
putBytes ws = fst (f ws)
  where f [] = (0, 0)
        f (w:[]) = (fromIntegral w, 8)
        f (w:ws) = let
          (w', x) = f ws
          in ((shiftL (fromIntegral w) x) .|. w', x + 8)

word64ByteCount :: Word64 -> Int
word64ByteCount key = if key <= 255 then 1 else 1 + word64ByteCount (shiftR key 8)

--------------------------------------------------------------------------------
-- Compress Code ---------------------------------------------------------------
--------------------------------------------------------------------------------

initialEncodeTable :: EncodeTable
initialEncodeTable = Table (M.fromAscList [(B.singleton x, fromIntegral x) | x <- [(0::Word8)..]]) (fromIntegral (maxBound :: Word8))

updateEncodeTable :: EncodeTable -> Chunk -> EncodeTable
updateEncodeTable (Table table maxKey) c = Table (M.insert c (maxKey + 1) table) (maxKey + 1)

compressKey :: Key -> EncodeTable -> [Word8]
compressKey k (Table _ maxKey) = takeBytes (word64ByteCount maxKey) k

-- Finds longest sequence from head of chunk in table
-- Nothing if Chunk lenght is 0
lookupMax :: EncodeTable -> Chunk -> Maybe (Chunk, Key)
lookupMax (Table table _) input = lastToMaybe (unfoldr (f table input) 1)
  where f table input len
          | len > B.length input = Nothing
          | otherwise = (M.lookup (B.take len input) table) >>= (\key -> Just (((B.take len input), key), len+1))

-- could use fold instead?
compress :: Chunk -> Chunk
compress original = B.pack (concat (unfoldr (\(t, i) -> (lookupMax t i) >>= (iterator t i)) (initialEncodeTable, original)))
  where iterator table input (found, key) =
          let remaining = B.drop (B.length found) input
              newEntry = B.take (1 + B.length found) input -- unless input == found
          in Just (compressKey key table, (updateEncodeTable table newEntry, remaining))

--------------------------------------------------------------------------------
-- Decompress Code--------------------------------------------------------------
--------------------------------------------------------------------------------

initialDecodeTable :: DecodeTable
initialDecodeTable = Table (M.fromAscList [(fromIntegral x, B.singleton x) | x <- [(0::Word8)..]]) (fromIntegral (maxBound :: Word8))

updateDecodeTable :: DecodeTable -> Chunk -> DecodeTable
updateDecodeTable (Table table maxKey) c = Table (M.insert (maxKey + 1) c table) (maxKey + 1)

makeKey :: Chunk -> Key
makeKey c = putBytes (B.unpack (B.dropWhile (== 0) c))

decodeKey :: Chunk -> Chunk -> DecodeTable -> Chunk
decodeKey key previous (Table table _) = fromMaybe (B.snoc previous (B.head previous)) (M.lookup (makeKey key) table)

-- Should return Nothing if input is less than maxKey
takeKey :: DecodeTable -> Chunk -> (Chunk, Chunk)
takeKey (Table _ maxKey) = B.splitAt (word64ByteCount (maxKey + 1))

--decompress :: Chunk -> Either String Chunk
decompress :: Chunk -> Chunk
decompress original = B.concat (first : (unfoldr iterator (initialDecodeTable, rest, first)))
  where
    first = B.take 1 original
    rest = B.drop 1 original
    iterator (table, input, previous)
      | B.null input = Nothing
      | otherwise = let
        (key, remaining) = takeKey table input
        output = decodeKey key previous table
        table' = updateDecodeTable table (B.snoc previous (B.head output))
        in Just (output, (table', remaining, output))
