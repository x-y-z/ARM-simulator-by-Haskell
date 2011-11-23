{-# LANGUAGE GeneralizedNewtypeDeriving #-}
----------------------------------------------------------------------
-- FILE:    BinaryNumber.hs
-- DATE:    11/10/2000
-- AUTHOR:  Jeffrey A. Meunier
-- EMAIL:   jeffm@cse.uconn.edu
----------------------------------------------------------------------


-- This module Arm.represents binary numbers which are read and displayed
-- as a sequence of bits.  A Binary32 number is semantically
-- equivalent to a Word32 number, so strictly speaking, all the extra
-- class information is not needed.



module Arm.BinaryNumber
  ( Binary32
  , intToBinary32             -- :: Int      -> Binary32
  , binary32ToInt             -- :: Binary32 -> Int
  , binary32ToWord32          -- :: Binary32 -> Word32
  , word32ToBinary32          -- :: Word32   -> Binary32
  )
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Word
import Data.Array
import Data.Ratio



----------------------------------------------------------------------
-- New type Binary32.
----------------------------------------------------------------------
newtype Binary32
  = B32 Word32
  deriving Num

instance Show Binary32 where
  showsPrec n (B32 wrd) = showString (biNumToString wrd "")
    where
      biNumToString 0 accum = ('0' : accum)
      biNumToString 1 accum = ('1' : accum)
      biNumToString n accum
        = if n `rem` 2 == 0
            then biNumToString (n `div` 2) ('0' : accum)
            else biNumToString (n `div` 2) ('1' : accum)

instance Read Binary32 where
  readsPrec n = (stringToBiNum 0)
    where
      stringToBiNum :: Word32 -> ReadS Binary32
      stringToBiNum acc "0" = [(B32 (acc * 2), "")]
      stringToBiNum acc "1" = [(B32 (acc * 2 + 1), "")]
      stringToBiNum acc (bit : bits)
        | bit == '0' = stringToBiNum (acc * 2) bits
        | bit == '1' = stringToBiNum (acc * 2 + 1) bits


{-

This expression also converts a binary string into an integer, but it uses
4.3 times the number of reductions, and 4.8 times the number of cells:

s2b x = foldl (+) 0 (map (uncurry (*)) (zip (reverse (map ((flip (-)) (ord '0')) (map ord x))) [floor (2 ** x) | x <- [0..]]))

-}


instance Eq Binary32 where
  (==) = binop (==)

instance Ord Binary32 where
  compare = binop compare

-- instance Num Binary32 where
--     x + y         = to (binop (+) x y)
--     x - y         = to (binop (-) x y)
--     negate        = to . negate . from
--     x * y         = to (binop (*) x y)
--     abs           = absReal
--     signum        = signumReal
--     -- fromInteger   = to . primIntegerToWord
--     -- fromInt       = intToBinary32

instance Bounded Binary32 where
    minBound = B32 0
    maxBound = B32 (maxBound :: Word32)

instance Real Binary32 where
    toRational x = toInteger x % 1

instance Integral Binary32 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    divMod        = quotRem
    toInteger     = toInteger . from
    -- toInt         = binary32ToInt

instance Ix Binary32 where
    range (m,n)          = [m..n]
    index b@(m,n) i
           | inRange b i = fromIntegral (from (i - m))
           | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Binary32 where
    toEnum         = to . fromIntegral
    fromEnum       = fromIntegral . from
    enumFrom       = numericEnumFrom
    enumFromTo     = numericEnumFromTo
    enumFromThen   = numericEnumFromThen
    enumFromThenTo = numericEnumFromThenTo

instance Bits Binary32 where
  x .&. y       = to (binop (.&.) x y)
  x .|. y       = to (binop (.|.) x y)
  x `xor` y     = to (binop xor x y)
  complement    = to . complement . from
  x `shift` i   = to (from x `shift` i)
--  rotate      
  bit           = to . bit
  setBit x i    = to (setBit (from x) i)
  clearBit x i  = to (clearBit (from x) i)
  complementBit x i = to (complementBit (from x) i)
  testBit x i   = testBit (from x) i
  bitSize  _    = 32
  isSigned _    = False



----------------------------------------------------------------------
-- Conversion functions.
----------------------------------------------------------------------
intToBinary32 :: Int -> Binary32
intToBinary32 = (B32 . fromIntegral)

binary32ToInt :: Binary32 -> Int
binary32ToInt (B32 b) = fromIntegral b

binary32ToWord32 :: Binary32 -> Word32
binary32ToWord32 (B32 b) = b

word32ToBinary32 :: Word32 -> Binary32
word32ToBinary32 = B32



-----------------------------------------------------------------------------
-- Enumeration code: copied from Prelude.
-----------------------------------------------------------------------------
numericEnumFrom        :: Real a => a -> [a]
numericEnumFromThen    :: Real a => a -> a -> [a]
numericEnumFromTo      :: Real a => a -> a -> [a]
numericEnumFromThenTo  :: Real a => a -> a -> a -> [a]
numericEnumFrom n            = n : (numericEnumFrom $! (n+1))
numericEnumFromThen n m      = iterate ((m-n)+) n
numericEnumFromTo n m        = takeWhile (<= m) (numericEnumFrom n)
numericEnumFromThenTo n n' m = takeWhile (if n' >= n then (<= m) else (>= m))
                                         (numericEnumFromThen n n')



-----------------------------------------------------------------------------
-- Coercions - used to make the instance declarations more uniform.
-----------------------------------------------------------------------------
class Coerce a where
  to   :: Word32 -> a
  from :: a -> Word32

instance Coerce Binary32 where
  from = binary32ToWord32
  to   = word32ToBinary32

binop :: Coerce word => (Word32 -> Word32 -> a) -> (word -> word -> a)
binop op x y = from x `op` from y

to2 :: Coerce word => (Word32, Word32) -> (word, word)
to2 (x,y) = (to x, to y)



-----------------------------------------------------------------------------
-- Primitives.
-----------------------------------------------------------------------------
-- primitive, primIntegerToWord :: Integer -> Word32



-----------------------------------------------------------------------------
-- Code copied from the Prelude.
-----------------------------------------------------------------------------
absReal x
  | x >= 0    = x
  | otherwise = -x

signumReal x
  | x == 0    =  0
  | x > 0     =  1
  | otherwise = -1



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
