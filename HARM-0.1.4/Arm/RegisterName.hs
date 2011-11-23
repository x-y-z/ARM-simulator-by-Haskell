----------------------------------------------------------------------
-- FILE:              RegisterName.hs
-- DATE:              2/6/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.RegisterName
  ( RegisterName(..)
  , nthReg
  )
where


import Data.Word
import Data.Array


----------------------------------------------------------------------
-- Data type for register names.
----------------------------------------------------------------------
data RegisterName
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | CPSR
  deriving (Enum, Eq, Ix, Ord, Read)

instance Show RegisterName where
  show r = show' r



----------------------------------------------------------------------
----------------------------------------------------------------------
nthReg :: Word32 -> RegisterName
nthReg  0 = R0
nthReg  1 = R1
nthReg  2 = R2
nthReg  3 = R3
nthReg  4 = R4
nthReg  5 = R5
nthReg  6 = R6
nthReg  7 = R7
nthReg  8 = R8
nthReg  9 = R9
nthReg 10 = R10
nthReg 11 = R11
nthReg 12 = R12
nthReg 13 = R13
nthReg 14 = R14
nthReg 15 = R15



----------------------------------------------------------------------
-- Convert a register name to a string.
----------------------------------------------------------------------
show' R0   = "r0"
show' R1   = "r1"
show' R2   = "r2"
show' R3   = "r3"
show' R4   = "r4"
show' R5   = "r5"
show' R6   = "r6"
show' R7   = "r7"
show' R8   = "r8"
show' R9   = "r9"
show' R10  = "r10"
show' R11  = "r11"
show' R12  = "r12"
show' R13  = "r13"
show' R14  = "r14"
show' R15  = "r15"
show' CPSR = "cpsr"



----------------------------------------------------------------------
-- Convert a string to a register name.
----------------------------------------------------------------------
read' "r0"   = R0
read' "r1"   = R1
read' "r2"   = R2
read' "r3"   = R3
read' "r4"   = R4
read' "r5"   = R5
read' "r6"   = R6
read' "r7"   = R7
read' "r8"   = R8
read' "r9"   = R9
read' "r10"  = R10
read' "r11"  = R11
read' "r12"  = R12
read' "r13"  = R13
read' "r14"  = R14
read' "r15"  = R15
read' "cpsr" = CPSR



----------------------------------------------------------------------
-- Convert register name to index (will be used to index register array).
----------------------------------------------------------------------
{-
index' R0   = 0
index' R1   = 1
index' R2   = 2
index' R3   = 3
index' R4   = 4
index' R5   = 5
index' R6   = 6
index' R7   = 7
index' R8   = 8
index' R9   = 9
index' R10  = 10
index' R11  = 11
index' R12  = 12
index' R13  = 13
index' R14  = 14
index' R15  = 15
index' CPSR = 16
-}


----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
