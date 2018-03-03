module Codec.Compression.LZW where
--module Codec.Compression.LZW.Internal () where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.List
import Data.Word
import Data.Bits
import Data.Maybe

data Bit = Zero | One

instance Show Bit where
  show Zero = "0"
  show One = "1"

instance Eq Bit where
  Zero == Zero = True
  One == One = True
  _ == _ = False

type Chunk = B.ByteString
type Key = Int
type BitStream = [Bit]
type EncodeTable = M.Map Chunk Key
type DecodeTable = M.Map Key Chunk

--------------------------------------------------------------------------------
-- Util ------------------------------------------------------------------------
--------------------------------------------------------------------------------

lastToMaybe :: [a] -> Maybe a
lastToMaybe [] = Nothing
lastToMaybe x  = Just (last x)

splitAt' 0 zs = Just ([],zs)
splitAt' n [] = Nothing
splitAt' n (z:zs)
  | n > 0     = fmap (\(zs',zs'') -> (z:zs',zs'')) (splitAt' (n-1) zs)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Conversions -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- shift . or
(#) :: (Bits a, Num a) => a -> Bit -> a
(#) w One = (shiftL w 1) .|. 1
(#) w Zero = shiftL w 1

packBits :: BitStream -> [Word8]
packBits (b8:b7:b6:b5:b4:b3:b2:b1:xs) = (0 # b8 # b7 # b6 # b5 # b4 # b3 # b2 # b1) : packBits xs
packBits (b8:b7:b6:b5:b4:b3:b2:[]) = [shiftL (0 # b8 # b7 # b6 # b5 # b4 # b3 # b2) 1]
packBits (b8:b7:b6:b5:b4:b3:[]) = [shiftL (0 # b8 # b7 # b6 # b5 # b4 # b3) 2]
packBits (b8:b7:b6:b5:b4:[]) = [shiftL (0 # b8 # b7 # b6 # b5 # b4) 3]
packBits (b8:b7:b6:b5:[]) = [shiftL (0 # b8 # b7 # b6 # b5) 4]
packBits (b8:b7:b6:[]) = [shiftL (0 # b8 # b7 # b6) 5]
packBits (b8:b7:[]) = [shiftL (0 # b8 # b7) 6]
packBits (b8:[]) = [shiftL (0 # b8) 7]
packBits [] = []

getBit :: (Bits a) => a -> Int -> Bit
getBit w n = if testBit w n then One else Zero

getBits :: (Bits a) => a -> Int -> [Bit]
getBits w n
  | n < 0 = []
  | otherwise = getBit w n : getBits w (n - 1)

toBits :: (Bits a, FiniteBits a) => a -> [Bit]
toBits w = getBits w ((finiteBitSize w) - 1)

unpackBits :: B.ByteString -> BitStream
unpackBits bs = concat (map toBits (B.unpack bs))

-- Returns the minimun number of bits needed to represent a given number
bitCount :: Int -> Int
bitCount n = finiteBitSize n - countLeadingZeros n

-- Converts a number of bits to a key
toKey :: [Bit] -> Key
toKey key = f key 0
  where f (k:ks)  n = f ks (n # k)
        f [] n = n

-- Converts a list of keys to a BitStream, for each key added increases the number of bits per key
packKeys :: [Key] -> BitStream
packKeys keys = f (zip [bitCount x | x <- [255..]] keys)
  where f [] = []
        f ((1, k):ks) = getBit k 0 : f ks
        f ((n, k):ks) = getBit k (n - 1) : f ((n - 1, k):ks)

-- Converts a bitstream to a list of keys, assumes first key has been extracted
unpackKeys :: BitStream -> [Key] -- for decoding
unpackKeys bs = f bs 256
  where
    f [] _ = []
    f xs a = case splitAt' (bitCount a) xs of
      Just (key, rest) -> toKey key : f rest (a + 1)
      _ -> []

--------------------------------------------------------------------------------
-- Compress Code ---------------------------------------------------------------
--------------------------------------------------------------------------------

initialEncodeTable :: EncodeTable
initialEncodeTable = M.fromDistinctAscList [ (B.singleton n, fromIntegral n) | n <- [(0::Word8)..] ]

updateEncodeTable :: Chunk -> EncodeTable -> EncodeTable
updateEncodeTable c t = M.insert c (M.size t) t

-- Finds longest sequence from head of chunk in table
-- Nothing if Chunk lenght is 0
lookupMax :: EncodeTable -> Chunk -> Maybe (Chunk, Key)
lookupMax table input = lastToMaybe (unfoldr (f table input) 1)
  where f table input len
          | len > B.length input = Nothing
          | otherwise = (M.lookup (B.take len input) table) >>= (\key -> Just (((B.take len input), key), len+1))

compress :: Chunk -> Chunk
compress input = B.pack (packBits (packKeys (compress' initialEncodeTable input)))

compress' :: EncodeTable -> Chunk -> [Key]
compress' table input
  | B.null input = []
  | otherwise = case lookupMax table input of
      Nothing -> [] -- ?
      Just (found, key) -> key : (compress' newTable remaining)
        where newEntry = B.take (1 + B.length found) input -- unless input == found
              newTable = updateEncodeTable newEntry table
              remaining = B.drop (B.length found) input
              
--------------------------------------------------------------------------------
-- Decompress Code--------------------------------------------------------------
--------------------------------------------------------------------------------

initialDecodeTable :: DecodeTable
initialDecodeTable = M.fromDistinctAscList [ (fromIntegral n, B.singleton n) | n <- [(0::Word8)..] ]

updateDecodeTable :: Chunk -> DecodeTable -> DecodeTable
updateDecodeTable c t = M.insert (M.size t) c t

getKey :: Key -> Chunk -> DecodeTable -> Chunk
getKey key previous table = fromMaybe (B.snoc previous (B.head previous)) (M.lookup key table)

decompress :: Chunk -> Chunk
decompress input = let (first, rest) = B.splitAt 1 input
  in B.concat (first : (decompress' initialDecodeTable (unpackKeys (unpackBits rest)) first))

decompress' :: DecodeTable -> [Key] -> Chunk -> [Chunk]
decompress' table [] _ = []
decompress' table (key:ks) previous = let
  output = getKey key previous table
  newTable = updateDecodeTable (B.snoc previous (B.head output)) table
  in output : (decompress' newTable ks output)
