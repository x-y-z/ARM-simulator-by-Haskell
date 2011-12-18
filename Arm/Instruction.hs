module Instruction
where

import Data.Word

import Control.Monad

import Test.QuickCheck

import Operand
import RegisterName

data Instruction
  = Add   Operand Operand Operand
  | And   Operand Operand Operand
  | B     Operand
  | Beq   Operand
  | Bgt   Operand
  | Bic   Operand Operand Operand
  | Blt   Operand
  | Bne   Operand
  | Cmp   Operand Operand
  | Eor   Operand Operand Operand
  | Ldr   Operand Operand
  | Mov   Operand Operand
  | Mul   Operand Operand Operand
  | Orr   Operand Operand Operand
  | Str   Operand Operand
  | Sub   Operand Operand Operand
  | Swi   Operand

instance Show Instruction where
  show (Add   op1 op2 op3) = "add   " ++ 
                             show op1 ++ ", " ++ 
                             show op2 ++ ", " ++ 
                             show op3
  show (And   op1 op2 op3) = "and   " ++ 
                             show op1 ++ ", " ++ 
                             show op2 ++ ", " ++ 
                             show op3
  show (B     op1)         = "b     " ++ show op1
  show (Beq   op1)         = "beq   " ++ show op1
  show (Bgt   op1)         = "bgt   " ++ show op1
  show (Bic   op1 op2 op3) = "bic   " ++ 
                             show op1 ++ ", " ++ 
                             show op2 ++ ", " ++ 
                             show op3
  show (Blt   op1)         = "blt   " ++ show op1
  show (Bne   op1)         = "bne   " ++ show op1
  show (Cmp   op1 op2)     = "cmp   " ++ 
                             show op1 ++ ", " ++ 
                             show op2
  show (Eor   op1 op2 op3) = "eor   " ++ 
                             show op1 ++ ", " ++ 
                             show op2 ++ ", " ++ 
                             show op3
  show (Ldr   op1 op2)     = "ldr   " ++ show op1 ++ ", " ++ show op2
  show (Mov   op1 op2)     = "mov   " ++ show op1 ++ ", " ++ show op2
  show (Mul   op1 op2 op3) = "mul   " ++ 
                             show op1 ++ ", " ++ 
                             show op2 ++ ", " ++ 
                             show op3
  show (Orr   op1 op2 op3) = "orr   " ++ 
                             show op1 ++ ", " ++ 
                             show op2 ++ ", " ++ 
                             show op3
  show (Str   op1 op2)     = "str   " ++ show op1 ++ ", " ++ show op2
  show (Sub   op1 op2 op3) = "sub   " ++ 
                             show op1 ++ ", " ++ 
                             show op2 ++ ", " ++ 
                             show op3
  show (Swi   op1)         = "swi   " ++ show op1
  
arbOffset :: Gen Word32
arbOffset = elements [0,4,8,12,16,20,24]
  
arbReg :: Gen Operand
arbReg = liftM Reg arbitrary
  
arbCon :: Gen Operand
arbCon = liftM Con (elements [0..200])

-- Only generating small offsets to not jump off of the end (or beginning)
-- too often
arbRel :: Gen Operand
arbRel = liftM Rel (elements [-4,-8,-12,-16,4,8,12,16,20,24])
         
arbInd :: Gen Operand
arbInd = liftM Ind arbitrary

arbBas :: Gen Operand
arbBas = liftM2 Bas arbitrary arbOffset

arbAut :: Gen Operand
arbAut = liftM Aut arbBas

arbPos :: Gen Operand
arbPos = liftM2 Pos arbInd arbOffset
         
-- Generating everything but Swi because we don't want to 
-- have to give command-line input during random testing
arbI :: Gen Instruction
arbI = frequency [ (3, liftM3 Add arbReg arbReg arbReg ), 
                   (4, liftM3 Add arbReg arbReg arbCon ), 
                   (1, liftM3 And arbReg arbReg arbReg ), 
                   (1, liftM B arbRel), 
                   (1, liftM Beq arbRel), 
                   (1, liftM Bgt arbRel),
                   (1, liftM3 Bic arbReg arbReg arbReg ), 
                   (1, liftM Blt arbRel), 
                   (1, liftM Bne arbRel),
                   (1, liftM2 Cmp arbReg arbCon), 
                   (2, liftM2 Cmp arbReg arbReg), 
                   (1, liftM3 Eor arbReg arbReg arbReg), 
                   (1, liftM2 Ldr arbReg arbInd), 
                   (1, liftM2 Ldr arbReg arbBas),
                   (1, liftM2 Ldr arbReg arbAut),
                   (1, liftM2 Ldr arbReg arbPos), 
                   (2, liftM2 Mov arbReg arbCon),
                   (2, liftM2 Mov arbReg arbReg),
                   (1, liftM3 Mul arbReg arbReg arbReg),
                   (1, liftM3 Orr arbReg arbReg arbReg), 
                   (1, liftM2 Str arbReg arbInd),
                   (1, liftM2 Str arbReg arbAut),
                   (1, liftM2 Str arbReg arbBas),
                   (1, liftM2 Str arbReg arbPos),
                   (1, liftM3 Sub arbReg arbReg arbReg) ]
  
instance Arbitrary Instruction where
  arbitrary = arbI
