{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-#OPTIONS -XMultiParamTypeClasses -XTypeSynonymInstances -XFlexibleContexts -XFunctionalDependencies -XFlexibleInstances -XUndecidableInstances#-}
module Register (
                 ----functions
                 getReg_, setReg_, 
                 ----data structures
                 Registers, emptyRegs,
                 ----classes
                 CRegisters,
                 prop_cpsr_get_set,
                 prop_reg_get_set,
                 getNsetReg
                )
where

-----------------------
-- system library
-----------------------

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits
import Data.Word
import Test.HUnit
import Test.QuickCheck

-------------------
-- user library
-------------------
import RegisterName



-------------------
-- classes
-------------------
class (Ord name, Show val) => CRegisters reg name val|reg->name, reg->val where
      emptyRegs_ :: reg
      getReg_ :: reg -> name -> val
      setReg_ :: reg -> name -> val -> reg
      cpsrGet_ :: reg -> Int -> val
      cpsrSet_ ::reg -> Int -> reg

-- | set Negative, Zero, Carry, oVerflowed bit in 
--   CPSR (current program status register)
      cpsrGetN_, cpsrGetZ_, cpsrGetC_, cpsrGetV_ :: reg -> val
      cpsrSetN_, cpsrSetZ_, cpsrSetC_, cpsrSetV_:: reg -> reg
      
      cpsrGetN_ rs = cpsrGet_ rs 31
      cpsrSetN_ rs = cpsrSet_ rs 31

      cpsrGetZ_ rs = cpsrGet_ rs 30
      cpsrSetZ_ rs = cpsrSet_ rs 30

      cpsrGetC_ rs = cpsrGet_ rs 29
      cpsrSetC_ rs = cpsrSet_ rs 29

      cpsrGetV_ rs = cpsrGet_ rs 28
      cpsrSetV_ rs = cpsrSet_ rs 28
      
      showCPSRFlags_ :: reg -> IO ()           
      showCPSRFlags_ rs = let n = cpsrGetN_ rs
                              z = cpsrGetZ_ rs
                              c = cpsrGetC_ rs
                              v = cpsrGetV_ rs
                          in putStr ("N=" ++ show n ++ 
                                    " Z=" ++ show z ++ 
                                    " C=" ++ show c ++ 
                                    " V=" ++ show v)


-----------------------
-- instances
-----------------------
type Registers = Map RegisterName Word32

-- Initialize Registers to value 0.  This way, Map.! always works.
emptyRegs :: Registers
emptyRegs = Map.fromList[
  (R0,0),
  (R1,0),
  (R2,0),
  (R3,0),
  (R4,0),
  (R5,0),
  (R6,0),
  (R7,0),
  (R8,0),
  (R9,0),
  (R10,0),
  (R11,0),
  (R12,0),
  (R13,0),
  (R14,0),
  (R15,0),
  (CPSR,0)
  ]

-- | instance for Registers 
instance CRegisters Registers RegisterName Word32  where
         emptyRegs_ = Map.fromList[(R0,0), (R1,0), (R2,0),
                                  (R3,0), (R4,0), (R5,0),
                                  (R6,0), (R7,0), (R8,0),
                                  (R9,0), (R10,0),(R11,0),
                                  (R12,0),(R13,0),(R14,0),
                                  (R15,0),(CPSR,0)]
         getReg_ rs idx = rs Map.! idx
         setReg_ rs idx val = Map.insert idx val rs
         cpsrGet_ rs bitx = if cpsr `testBit` bitx then 1 else 0
           where cpsr = getReg_ rs CPSR
         cpsrSet_ rs bitx = let cpsr = getReg_ rs CPSR
                                cpsr' = cpsr `setBit` bitx
                            in setReg_ rs CPSR cpsr'

----------------------
-- test
----------------------

-- | unit test for getReg_ and setReg_
getNsetReg :: Test
getNsetReg = TestList [ getReg_ (setReg_ emptyRegs R1 (99::Word32)) R1 ~?= 
                        (99::Word32)
                        ]

-- | quickCheck for getReg_ and setReg_
prop_reg_get_set :: RegisterName -> Word32 -> Property
prop_reg_get_set r v = property $ (getReg_ (setReg_ emptyRegs r v)) r == v

-- | quickCheck for CPSR get and set
prop_cpsr_get_set :: Int -> Property
prop_cpsr_get_set v = property $ (cpsrGet_ (cpsrSet_ emptyRegs v')) v' == 1
     where v' :: Int
           v' =  v `mod` 32 
           