----------------------------------------------------------------------
-- FILE:              Register.hs
-- DATE:              1/6/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Register
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
-- import IOExts
import Data.Bits
import Data.Word
import Data.Array.IO



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.RegisterName



----------------------------------------------------------------------
-- This is the register set type.
----------------------------------------------------------------------
type Registers
  = IOArray RegisterName Word32



----------------------------------------------------------------------
-- Create a new set of empty registers.
----------------------------------------------------------------------
emptyRegs
  :: IO Registers

emptyRegs
  = newArray (R0, CPSR) 0



----------------------------------------------------------------------
-- Get the value in a register.
----------------------------------------------------------------------
getReg
  :: Registers
  -> RegisterName
  -> IO Word32

getReg regs regName
  = readArray regs regName



----------------------------------------------------------------------
-- Set a register with a new value.
----------------------------------------------------------------------
setReg
  :: Registers
  -> RegisterName
  -> Word32
  -> IO ()

setReg regs regName regVal
  = writeArray regs regName regVal



----------------------------------------------------------------------
-- CPSR functions.
----------------------------------------------------------------------

showCPSRFlags regs
  = do n <- cpsrGetN regs
       z <- cpsrGetZ regs
       c <- cpsrGetC regs
       v <- cpsrGetV regs
       putStr ("N=" ++ show n ++ " Z=" ++ show z ++ " C=" ++ show c ++ " V=" ++ show v)

cpsrGetN = cpsrGet 31
cpsrSetN = cpsrSet 31

cpsrGetZ = cpsrGet 30
cpsrSetZ = cpsrSet 30

cpsrGetC = cpsrGet 29
cpsrSetC = cpsrSet 29

cpsrGetV = cpsrGet 28
cpsrSetV = cpsrSet 28

cpsrGet
  :: Int
  -> Registers
  -> IO Word32

cpsrGet bit regs
  = do cpsr <- getReg regs CPSR
       if cpsr `testBit` bit
         then return 1
         else return 0

cpsrSet
  :: Int
  -> Registers
  -> IO ()

cpsrSet bit regs
  = do cpsr <- getReg regs CPSR
       let cpsr' = cpsr `setBit` bit
       setReg regs CPSR cpsr'



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
