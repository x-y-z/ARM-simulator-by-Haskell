----------------------------------------------------------------------
-- FILE:              Program.hs
-- DATE:              03/07/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Program
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Instruction
import Arm.Memory
import Arm.RegisterName



----------------------------------------------------------------------
-- Constant data type.  This allows us to represent constant
-- data values in our program (although when the program runs, the
-- values can potentially change, so they are not really constants).
----------------------------------------------------------------------
data Constant
  = Array Word32 Constant
  | Int Int
  | List [Constant]
  | String String
  | Word Word32
  deriving Show



----------------------------------------------------------------------
-- Get the size of a constant.
----------------------------------------------------------------------
constSize
  :: Constant
  -> Word32

constSize (Array i c) = i * constSize c
constSize (Int _)     = 4
constSize (List l)    = foldl (+) 0 (map constSize l)
constSize (String s)  = fromIntegral ((length s `div` 4 + 1) * 4)
constSize (Word _)    = 4



----------------------------------------------------------------------
-- Program data type.  A program has an origin, a list of instructions,
-- and a list of constants.
----------------------------------------------------------------------
data Program
  = Program
      { memorySize   :: Address                  -- required number of bytes
      , origin       :: Address                  -- program origin
      , regInit      :: [(RegisterName, Word32)] -- initial register values
      , instructions :: [Instruction]            -- list of instructions
      , constants    :: [(Address, Constant)]    -- list of constants
      }
  deriving Show



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
