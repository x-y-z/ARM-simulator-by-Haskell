{-#OPTIONS  -XFlexibleContexts #-}

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

setAuxilary :: (MonadState CPU m, MonadIO m) => Auxilary -> m ()
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
getCounter :: (MonadState CPU m, MonadIO m) => String -> m Integer
getCounter id = do (CPU _ _ cs _) <- get
                   if Map.member id cs then return $ cs Map.! id else return 0

setCounter :: (MonadState CPU m, MonadIO m) => String -> Integer -> m ()
setCounter id cnt = do (CPU m rs cs aux) <- get
                       put $ CPU m rs (Map.insert id cnt cs) aux

currentCycle :: (MonadState CPU m, MonadIO m) => m Integer
currentCycle = getCounter "Cycles"

nextCycle :: (MonadState CPU m, MonadIO m) => m ()
nextCycle = do cyc <- getCounter "Cycles"
               setCounter "Cycles" (cyc + 1)

startRunning :: (MonadState CPU m, MonadIO m) => m ()
startRunning = setCounter "Running" 1

stopRunning :: (MonadState CPU m, MonadIO m) => m ()
stopRunning = setCounter "Running" 0

isRunning :: (MonadState CPU m, MonadIO m) => m Bool
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
getReg :: (MonadState CPU m, MonadIO m) => RegisterName -> m Word32
getReg id = do (CPU _ rs _ _) <- get
               return $ rs Map.! id

----------------------------------------------------------------------
-- Set a register with a new value.
----------------------------------------------------------------------
setReg :: (MonadState CPU m, MonadIO m) => RegisterName -> Word32 -> m ()
setReg id val = do (CPU mem rs cs aux) <- get
                   put (CPU mem (Map.insert id val rs) cs aux)

cpsrGetN :: (MonadState CPU m, MonadIO m) => m Word32
cpsrGetN = cpsrGet 31

cpsrSetN ::(MonadState CPU m, MonadIO m) => m ()
cpsrSetN = cpsrSet 31

cpsrGetZ :: (MonadState CPU m, MonadIO m) => m Word32
cpsrGetZ = cpsrGet 30

cpsrSetZ :: (MonadState CPU m, MonadIO m) => m ()
cpsrSetZ = cpsrSet 30

cpsrGetC ::(MonadState CPU m, MonadIO m) => m Word32
cpsrGetC = cpsrGet 29

cpsrSetC :: (MonadState CPU m, MonadIO m) => m ()
cpsrSetC = cpsrSet 29

cpsrGetV :: (MonadState CPU m, MonadIO m) => m Word32
cpsrGetV = cpsrGet 28

cpsrSetV :: (MonadState CPU m, MonadIO m) => m ()
cpsrSetV = cpsrSet 28

cpsrGet :: (MonadState CPU m, MonadIO m) => Int -> m Word32
cpsrGet bit = do cpsr <- getReg CPSR
                 if cpsr `testBit` bit then return 1 else return 0

cpsrSet :: (MonadState CPU m, MonadIO m) => Int -> m ()
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

getMemWord :: (MonadState CPU m, MonadIO m) => WordAddress -> m Word32
getMemWord addr = do (CPU m _ _ _) <- get
                     if Map.member addr m 
                       then return (m Map.! addr) else return 0

setMemWord :: (MonadState CPU m, MonadIO m) => WordAddress -> Word32 -> m ()
setMemWord addr val = do (CPU m rs cs aux) <- get
                         put $ (CPU (Map.insert addr val m) rs cs aux)

readMem :: (MonadState CPU m, MonadIO m) => Address -> m Word32
readMem byteAddr = getMemWord (wordAddress byteAddr)
    
writeMem :: (MonadState CPU m, MonadIO m) => Address -> Word32 -> m ()
writeMem byteAddr val = setMemWord (wordAddress byteAddr) val