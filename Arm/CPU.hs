module CPU
where
  
import Data.Map (Map)  
import qualified Data.Map as Map
import Data.Bits
import Data.Word

import Control.Monad.State

import RegisterName  

data CPU = CPU Memory Registers
  deriving Show

getMem :: CPU -> Memory
getMem (CPU memory _) = memory

getRegs :: CPU -> Registers
getRegs (CPU _ regs) = regs

-----------------------------------------------
-- Register Functions
-----------------------------------------------

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
  (CPSR,0),
  (PC,0)
  ]
  
----------------------------------------------------------------------
-- Get the value in a register.
----------------------------------------------------------------------
getReg :: RegisterName -> State CPU Word32
getReg id = do (CPU _ rs) <- get
               return $ rs Map.! id

----------------------------------------------------------------------
-- Set a register with a new value.
----------------------------------------------------------------------
setReg :: RegisterName -> Word32 -> State CPU ()
setReg id val = do (CPU mem rs) <- get
                   put (CPU mem (Map.insert id val rs))

cpsrGetN :: State CPU Word32
cpsrGetN = cpsrGet 31

cpsrSetN :: State CPU ()
cpsrSetN = cpsrSet 31

cpsrGetZ :: State CPU Word32
cpsrGetZ = cpsrGet 30

cpsrSetZ :: State CPU ()
cpsrSetZ = cpsrSet 30

cpsrGetC :: State CPU Word32
cpsrGetC = cpsrGet 29

cpsrSetC :: State CPU ()
cpsrSetC = cpsrSet 29

cpsrGetV :: State CPU Word32
cpsrGetV = cpsrGet 28

cpsrSetV :: State CPU ()
cpsrSetV = cpsrSet 28

cpsrGet :: Int -> State CPU Word32
cpsrGet bit = do cpsr <- getReg CPSR
                 if cpsr `testBit` bit then return 1 else return 0

cpsrSet :: Int -> State CPU ()
cpsrSet bit = do cpsr <- getReg CPSR
                 let cpsr' = cpsr `setBit` bit
                 setReg CPSR cpsr'
                 
------------------------------------------
-- Memory Functions
------------------------------------------

type Memory = Map Address Word32

type Address = Word32

type WordAddress = Address

type ByteAddress = Address

emptyMem :: Memory
emptyMem = Map.empty

wordAddress :: ByteAddress -> WordAddress
wordAddress addr = addr `div` 4

getMemWord :: WordAddress -> State CPU Word32
getMemWord addr = do (CPU m _) <- get
                     if Map.member addr m 
                       then return (m Map.! addr) else return 0

setMemWord :: WordAddress -> Word32 -> State CPU ()
setMemWord addr val = do (CPU m rs) <- get
                         put $ (CPU (Map.insert addr val m) rs)

readMem :: Address -> State CPU Word32
readMem byteAddr = getMemWord (wordAddress byteAddr)
    
writeMem :: Address -> Word32 -> State CPU ()
writeMem byteAddr val = setMemWord (wordAddress byteAddr) val