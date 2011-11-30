module Program
where

import Data.Word

import Instruction
import Memory
import RegisterName

data Constant
  = Array Word32 Constant
  | Int Int
  | List [Constant]
  | String String
  | Word Word32
  deriving Show

constSize
  :: Constant
  -> Word32

constSize (Array i c) = i * constSize c
constSize (Int _)     = 4
constSize (List l)    = foldl (+) 0 (map constSize l)
constSize (String s)  = fromIntegral ((length s `div` 4 + 1) * 4)
constSize (Word _)    = 4

data Program
  = Program
      { memorySize   :: Address                  -- required number of bytes
      , origin       :: Address                  -- program origin
      , regInit      :: [(RegisterName, Word32)] -- initial register values
      , instructions :: [Instruction]            -- list of instructions
      , constants    :: [(Address, Constant)]    -- list of constants
      }
  deriving Show