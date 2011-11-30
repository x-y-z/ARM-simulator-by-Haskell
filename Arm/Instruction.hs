module Instruction
where

import Data.Word

import Operand
import RegisterName

data Instruction
  = Add   Operand Operand Operand
  | And   Operand Operand Operand
  | B     Operand
  | Beq   Operand
  | Bgt   Operand
  | Bic   Operand Operand Operand
  | Bl    Operand
  | Blt   Operand
  | Bne   Operand
  | Cmp   Operand Operand
  | Eor   Operand Operand Operand
  | Ldmea Operand Operand
  | Ldr   Operand Operand
  | Ldrb  Operand Operand
  | Mov   Operand Operand
  | Mul   Operand Operand Operand
  | Orr   Operand Operand Operand
  | Stmea Operand Operand
  | Str   Operand Operand
  | Strb  Operand Operand
  | Sub   Operand Operand Operand
  | Swi   Operand

instance Show Instruction where
  show (Add   op1 op2 op3) = "add   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (And   op1 op2 op3) = "and   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (B     op1)         = "b     " ++ show op1
  show (Beq   op1)         = "beq   " ++ show op1
  show (Bgt   op1)         = "bgt   " ++ show op1
  show (Bic   op1 op2 op3) = "bic   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Bl    op1)         = "bl    " ++ show op1
  show (Blt   op1)         = "blt   " ++ show op1
  show (Bne   op1)         = "bne   " ++ show op1
  show (Cmp   op1 op2)     = "cmp   " ++ show op1 ++ ", " ++ show op2
  show (Eor   op1 op2 op3) = "eor   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Ldmea op1 op2)     = "ldmea " ++ show op1 ++ ", " ++ show op2
  show (Ldr   op1 op2)     = "ldr   " ++ show op1 ++ ", " ++ show op2
  show (Ldrb  op1 op2)     = "ldrb  " ++ show op1 ++ ", " ++ show op2
  show (Mov   op1 op2)     = "mov   " ++ show op1 ++ ", " ++ show op2
  show (Mul   op1 op2 op3) = "mul   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Orr   op1 op2 op3) = "orr   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Stmea op1 op2)     = "stmea " ++ show op1 ++ ", " ++ show op2
  show (Str   op1 op2)     = "str   " ++ show op1 ++ ", " ++ show op2
  show (Strb  op1 op2)     = "strb  " ++ show op1 ++ ", " ++ show op2
  show (Sub   op1 op2 op3) = "sub   " ++ show op1 ++ ", " ++ show op2 ++ ", " ++ show op3
  show (Swi   op1)         = "swi   " ++ show op1