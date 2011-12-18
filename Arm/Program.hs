{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module Program
where

import Data.Word

import Control.Monad

import Test.QuickCheck

import Instruction
import Operand
import Memory (Address)
import RegisterName

data Constant
  = Array Word32 Constant
  | Int Int
  | List [Constant]
  | String String
  | Word Word32
  deriving Show

instance Arbitrary Constant where
  arbitrary = frequency [ (9, liftM Word arbitrary),
                          (4, liftM Int arbitrary),
                          (1, liftM List arbitrary) ]

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
           
arbInit :: Gen Word32
arbInit = elements [0..200]

arbProg :: Gen Program
arbProg = do regs <- arbitrary
             inst <- arbitrary
             return $ fixTestProgram (Program 0 0 regs inst [])

-- Takes an arbitrary test program and fixes its addresses, offsets, etc.
fixTestProgram :: Program -> Program
fixTestProgram (Program ms o ri inst cs) = (Program ms o ri binst cs)
  where
    -- Adding a clean exit at the end of the program
    ninst = inst ++ [(Swi (Con 11))]
    l = (length ninst * 4) - 8
    binst = f ninst 0
    f [] _       = []
    f (i : is) n = (case i of 
                       (B (Rel off)) -> (B (Rel (fixOffset off l n)))
                       (Beq (Rel off)) -> (Beq (Rel (fixOffset off l n)))
                       (Bgt (Rel off)) -> (Bgt (Rel (fixOffset off l n)))
                       (Bne (Rel off)) -> (Bne (Rel (fixOffset off l n)))
                       _           -> i) : f is (n+1)
                   
-- Need to align branches so that we don't branch out of 
-- the instruction range
fixOffset :: Int -> Int -> Int -> Int
fixOffset o l n = if x > l then y else 
                    if x < 0 then z else x
  where
    x = (n * 4) + o
    y = l - x
    z = 0 - x
                                         
instance Arbitrary Program where
  arbitrary = arbProg