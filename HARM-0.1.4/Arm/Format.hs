----------------------------------------------------------------------
-- FILE:              Format.hs
-- DATE:              03/30/2001
-- PROJECT:           
-- LANGUAGE PLATFORM: 
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Format
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Array
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------



----------------------------------------------------------------------
-- Number base data type.
----------------------------------------------------------------------
data Radix
  = Dec
  | Hex
  deriving (Show)



----------------------------------------------------------------------
-- Format a number in a specific number base.
----------------------------------------------------------------------
formatNum base
  = case base of
      Dec -> formatDec 10 '0'
      Hex -> formatHex 8 '0' ""



----------------------------------------------------------------------
-- Convert a number to a hex string.
----------------------------------------------------------------------
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



----------------------------------------------------------------------
-- Array of hex characters.
----------------------------------------------------------------------
hexChars
  :: Array Word32 Char

hexChars
  = listArray (0, 15) "0123456789ABCDEF"



----------------------------------------------------------------------
-- Format a decimal integer
----------------------------------------------------------------------
formatDec
  :: Int
  -> Char
  -> Word32
  -> String

formatDec places fillChar n
  = let s = show n
        pad = places - (length s)
    in (take pad (repeat fillChar)) ++ s






----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
