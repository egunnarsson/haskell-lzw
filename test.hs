import Codec.Compression.LZW
import Test.QuickCheck
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import Data.Bits

instance Arbitrary B.ByteString where arbitrary = B.pack <$> arbitrary
instance CoArbitrary B.ByteString where coarbitrary = coarbitrary . B.unpack

fromBool b = if b then One else Zero
toBool One = True
toBool Zero = False

instance Arbitrary Bit where arbitrary = fromBool <$> arbitrary
instance CoArbitrary Bit where coarbitrary = coarbitrary . toBool

--------------------------------------------------------------------------------
-- Util ------------------------------------------------------------------------
--------------------------------------------------------------------------------

prop_lastToMaybe x@[] = (lastToMaybe x) == Nothing
prop_lastToMaybe x = (lastToMaybe x) /= Nothing

prop_splitAt (NonNegative n) xs = case splitAt' n xs of
    Nothing -> n > length xs
    Just x  -> x == splitAt n xs

--------------------------------------------------------------------------------
-- Conversions -----------------------------------------------------------------
--------------------------------------------------------------------------------

prop_shiftOr_BitSet x b@One = ((x # b) Data.Bits..&. 1) == 1
prop_shiftOr_BitSet x b@Zero = ((x # b) Data.Bits..&. 1) == 0

-- alot of anoying properties of shifting
--prop_shiftOr_Tail x b = ((x # b) shiftR 1) == (x Data.Bits..&. (minBound - 1))

prop_packBits_Length bs = length (packBits bs) == (div l 8) + m
    where l = length bs
          m = if mod l 8 == 0 then 0 else 1

prop_getBits_Length x (NonNegative n) = length (getBits x n) == (n + 1)

prop_getBits_Bits x (NonNegative n) = all (\(b, i) -> getBit x i == b) (zip (getBits x n) [x | x <- [0..]])

--------------------------------------------------------------------------------
-- Compress Code ---------------------------------------------------------------
--------------------------------------------------------------------------------

prop_compress_decompress x = x == (decompress (compress x))
