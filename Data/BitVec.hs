-- | Unsigned bit vectors.
module Data.BitVec
  ( BitVec
  , bitVec
  , select
  , width
  , value
  ) where

import Data.Bits
import Data.Semigroup

data BitVec = BitVec Int Integer deriving (Show, Eq)

instance Num BitVec where
  BitVec w1 v1 + BitVec w2 v2 = bitVec (max w1 w2) (v1 + v2)
  BitVec w1 v1 - BitVec w2 v2 = bitVec (max w1 w2) (v1 - v2)
  BitVec w1 v1 * BitVec w2 v2 = bitVec (max w1 w2) (v1 * v2)
  abs = id
  signum (BitVec _ v) = if v == 0 then bitVec 1 0 else bitVec 1 1
  fromInteger i = bitVec (width i) i
    where
    width :: Integer -> Int
    width a
      | a ==  0   = 0
      | a == -1   = 1
      | otherwise = 1 + width (shiftR a 1)

instance Bits BitVec where
  BitVec w1 v1 .&.   BitVec w2 v2 = bitVec (max w1 w2) (v1 .&.   v2)
  BitVec w1 v1 .|.   BitVec w2 v2 = bitVec (max w1 w2) (v1 .|.   v2)
  BitVec w1 v1 `xor` BitVec w2 v2 = bitVec (max w1 w2) (v1 `xor` v2)
  complement (BitVec w v) = bitVec w $ complement v
  shift (BitVec w v) i = bitVec w $ shift v i
  rotate _ _ = undefined --XXX  To lazy to implemented it now.
  bit i = fromInteger $ bit i
  testBit (BitVec _ v) i = testBit v i
  bitSize (BitVec w _) = w
  bitSizeMaybe (BitVec w _) = Just w
  isSigned _ = False
  popCount (BitVec _ v) = popCount v

instance Semigroup BitVec where
  (<>) = mappend

instance Monoid BitVec where
  mempty = BitVec 0 0
  mappend (BitVec w1 v1) (BitVec w2 v2) = BitVec (w1 + w2) (shiftL v1 w2 .|. v2)

-- | BitVec construction, given width and value.
bitVec :: Int -> Integer -> BitVec
bitVec w v = BitVec w' $ v .&. ((2 ^ fromIntegral w') - 1)
  where
  w' = max w 0

-- | Bit seclection.  LSB is 0.
select :: BitVec -> (BitVec, BitVec) -> BitVec
select (BitVec _ v) (msb, lsb) = bitVec (fromIntegral $ value $ msb - lsb + 1) $ shiftR v (fromIntegral $ value $ lsb)

-- | Width of a 'BitVec'.
width :: BitVec -> Int
width (BitVec w _) = w

-- | Value of a 'BitVec'.
value :: BitVec -> Integer
value (BitVec _ v) = v

