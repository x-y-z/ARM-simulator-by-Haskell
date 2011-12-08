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

type Debug = Bool


data CPU = CPU Memory Registers Debug Counters Auxilary
  deriving Show

getMem :: CPU -> Memory
getMem (CPU memory _ _ _ _) = memory

getRegs :: CPU -> Registers
getRegs (CPU _ regs _ _ _) = regs

getDbg :: CPU -> Debug
getDbg (CPU _ _ dbg _ _) = dbg

setDbg :: (MonadState CPU m, MonadIO m) => Debug -> m ()
setDbg dbg = do (CPU m rs _ cs aux) <- get
                put $ CPU m rs dbg cs aux

getCounters :: CPU -> Counters
getCounters (CPU _ _ _ counts _) = counts

getAuxilary :: CPU -> Auxilary
getAuxilary (CPU _ _ _ _ aux) = aux

setAuxilary :: (MonadState CPU m, MonadIO m) => Auxilary -> m ()
setAuxilary aux = do (CPU m rs dbg cs _) <- get
                     put $ CPU m rs dbg cs aux

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
getCounter id = do (CPU _ _ _ cs _) <- get
                   if Map.member id cs then return $ cs Map.! id else return 0

setCounter :: (MonadState CPU m, MonadIO m) => String -> Integer -> m ()
setCounter id cnt = do (CPU m rs dbg cs aux) <- get
                       put $ CPU m rs dbg (Map.insert id cnt cs) aux

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
               return $ r == 1

isDebug :: (MonadState CPU m, MonadIO m) => m Bool
isDebug = do (CPU _ _ dbg _ _) <- get
             return dbg
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
getReg id = do (CPU _ rs _ _ _) <- get
               return $ rs Map.! id

----------------------------------------------------------------------
-- Set a register with a new value.
----------------------------------------------------------------------
setReg :: (MonadState CPU m, MonadIO m) => RegisterName -> Word32 -> m ()
setReg id val = do (CPU mem rs dbg cs aux) <- get
                   put (CPU mem (Map.insert id val rs) dbg cs aux)

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
showCPSRFlags :: (MonadState CPU m, MonadIO m) => m ()                 
showCPSRFlags = do n <- cpsrGetN
                   z <- cpsrGetZ
                   c <- cpsrGetC
                   v <- cpsrGetV
                   liftIO $ putStr ("N=" ++ show n ++ " Z=" ++ show z ++ " C=" ++ show c ++ " V=" ++ show v)
------------------------------------------
-- Memory Functions
------------------------------------------

data Segment = CodeS | DataS | StackS | HeapS deriving (Ord, Eq, Show)

data Bound = Bound {lowerB :: Address, 
                    upperB :: Address} deriving Show

type MemLayout = Map Segment Bound

initMemLayout :: MemLayout
initMemLayout = Map.fromList [(CodeS, Bound 0 0), (DataS, Bound 0 0), (StackS, Bound 0 0), (HeapS, Bound 0 0)]

getBound :: MemLayout -> Segment -> (Address, Address)
getBound mly seg = case (Map.lookup seg mly) of
                        Just (Bound l u) -> (l, u)
                        Nothing          -> error "segment fault"

setBound :: MemLayout -> Segment -> (Address, Address) -> MemLayout
setBound mly seg (l, u) = case (Map.lookup seg mly) of
                               Just (Bound _ _) -> Map.insert seg (Bound l u) mly
                               Nothing          -> error "segment fault"

setBoundM :: (MonadState CPU m, MonadIO m) => Segment -> (Address, Address) -> m ()
setBoundM seg bnd = do (CPU m rs dbg cs aux) <- get
                       put $ (CPU (Memory (setBound (layout m) seg bnd) (mem m)) rs dbg cs aux)

getBoundM ::  (MonadState CPU m, MonadIO m) => Segment -> m (Address, Address)
getBoundM seg = do (CPU m rs dbg cs aux) <- get
                   return (getBound (layout m) seg)

data Memory = Memory { layout :: MemLayout,
                       mem    ::Map Address Word32} deriving (Show)

type Address = Word32

type WordAddress = Address

type ByteAddress = Address

emptyMem :: Memory
emptyMem = Memory initMemLayout Map.empty

wordAddress :: ByteAddress -> WordAddress
wordAddress addr = addr `div` 4

getMemWord :: (MonadState CPU m, MonadIO m) => WordAddress -> m Word32
getMemWord addr = do (CPU m _ _ _ _) <- get
                     if Map.member addr (mem m) 
                       then return ((mem m) Map.! addr) else return 0

setMemWord :: (MonadState CPU m, MonadIO m) => WordAddress -> Word32 -> m ()
setMemWord addr val = do (CPU m rs dbg cs aux) <- get
                         put $ (CPU (Memory (layout m) (Map.insert addr val (mem m))) rs dbg cs aux)

readMem :: (MonadState CPU m, MonadIO m) => Address -> m Word32
readMem byteAddr = getMemWord (wordAddress byteAddr)
    
writeMem :: (MonadState CPU m, MonadIO m) => Address -> Word32 -> m ()
writeMem byteAddr val = setMemWord (wordAddress byteAddr) val
