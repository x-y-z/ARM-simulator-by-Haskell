module Format
where

import Data.Array
import Data.Word

data Radix
  = Dec
  | Hex
  deriving (Show)

formatNum base
  = case base of
      Dec -> formatDec 10 '0'
      Hex -> formatHex 8 '0' ""

formatHex
  :: Int
  -> Char
  -> String
  -> Word32
  -> String

formatHex places fillChar accum n
  = if places == 0
      then accum
      else let digIndex = n `mod` 16
               dig = if n == 0
                       then fillChar
                       else hexChars ! digIndex
           in formatHex (places - 1) fillChar (dig : accum) (n `div` 16)

hexChars
  :: Array Word32 Char

hexChars
  = listArray (0, 15) "0123456789ABCDEF"

formatDec
  :: Int
  -> Char
  -> Word32
  -> String

formatDec places fillChar n
  = let s = show n
        pad = places - (length s)
    in (take pad (repeat fillChar)) ++ s