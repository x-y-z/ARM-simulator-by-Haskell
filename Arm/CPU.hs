module CPU
where
  
import Data.Map (Map)  
import qualified Data.Map as Map
import Data.Bits
import Data.Word

import Control.Monad.State

import Instruction
import RegisterName  

data CPU = CPU Memory Registers Counters Auxilary
  deriving Show

getMem :: CPU -> Memory
getMem (CPU memory _ _ _) = memory

getRegs :: CPU -> Registers
getRegs (CPU _ regs _ _) = regs

getCounters :: CPU -> Counters
getCounters (CPU _ _ counts _) = counts

getAuxilary :: CPU -> Auxilary
getAuxilary (CPU _ _ _ aux) = aux

setAuxilary :: Auxilary -> State CPU ()
setAuxilary aux = do (CPU m rs cs _) <- get
                     put $ CPU m rs cs aux

emptyAux :: Auxilary
emptyAux = InO [] []

data Auxilary = 
    Nil
  | InO {fd :: [Word32], de :: [Instruction]}
  deriving Show

type Counters = Map String Integer

emptyCounters :: Counters
emptyCounters = Map.insert "Cycles" 0 Map.empty

-- Return 0 when counter name doesn't exist
getCounter :: String -> State CPU Integer
getCounter id = do (CPU _ _ cs _) <- get
                   if Map.member id cs then return $ cs Map.! id else return 0

setCounter :: String -> Integer -> State CPU ()
setCounter id cnt = do (CPU m rs cs aux) <- get
                       put $ CPU m rs (Map.insert id cnt cs) aux

currentCycle :: State CPU Integer
currentCycle = getCounter "Cycles"

nextCycle :: State CPU ()
nextCycle = do cyc <- getCounter "Cycles"
               setCounter "Cycles" (cyc + 1)

startRunning :: State CPU ()
startRunning = setCounter "Running" 1

stopRunning :: State CPU ()
stopRunning = setCounter "Running" 0

isRunning :: State CPU Bool
isRunning = do r <- getCounter "Running"
               return $ r == 0

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
getReg id = do (CPU _ rs _ _) <- get
               return $ rs Map.! id

----------------------------------------------------------------------
-- Set a register with a new value.
----------------------------------------------------------------------
setReg :: RegisterName -> Word32 -> State CPU ()
setReg id val = do (CPU mem rs cs aux) <- get
                   put $ CPU mem (Map.insert id val rs) cs aux

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
getMemWord addr = do (CPU m _ _ _) <- get
                     if Map.member addr m 
                       then return (m Map.! addr) else return 0

setMemWord :: WordAddress -> Word32 -> State CPU ()
setMemWord addr val = do (CPU m rs cs aux) <- get
                         put $ (CPU (Map.insert addr val m) rs cs aux)

readMem :: Address -> State CPU Word32
readMem byteAddr = getMemWord (wordAddress byteAddr)
    
writeMem :: Address -> Word32 -> State CPU ()
writeMem byteAddr val = setMemWord (wordAddress byteAddr) val