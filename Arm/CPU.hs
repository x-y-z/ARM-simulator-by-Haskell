{-#OPTIONS  -XFlexibleContexts #-}

module CPU (Address, CPU(CPU), setReg, setBoundM, Segment(CodeS, DataS), writeMem,
            Debug, getReg, stopRunning, readMem, cpsrGetZ, cpsrGetC, cpsrGetN, 
            cpsrSetN, cpsrSetZ, cpsrSetC, getDbg, flushPipeline, queueLoad,
            queueStore, Auxilary(Nil, InO), setAuxilary, getLoad, nextCycle, 
            currentCycle, stallCycle, getStore, isRunning, Hierarchy, emptyMem,
            emptyRegs, emptyCounters, emptyAux, startRunning, Memory(Mem), getBoundM,
            showCPSRFlags, standardCache)
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

flushPipeline :: (MonadState CPU m, MonadIO m) => m ()
flushPipeline = setAuxilary emptyAux

emptyAux :: Auxilary
emptyAux = InO [] [] [] []

data Auxilary = 
    Nil
  | InO {fd :: [Word32], de :: [Instruction], em :: [(RegisterName,Address)], ew :: [(RegisterName,Address)]}
  deriving Show

queueLoad :: (MonadState CPU m, MonadIO m) => RegisterName -> Address -> m ()
queueLoad reg addr = do (CPU _ _ _ _ aux) <- get
                        case aux of
                          Nil -> fail "Need In-order auxilary data"
                          (InO fd de em ew) -> 
                            setAuxilary (InO fd de 
                                         (em ++ [(reg,addr)]) ew)
                                               
getLoad :: (MonadState CPU m, MonadIO m) => m (Maybe (RegisterName,Address))
getLoad = do (CPU _ _ _ _ aux) <- get
             case aux of
               Nil -> fail "Need In-order auxilary data"
               (InO _ _ [] _) -> return Nothing
               (InO fd de (x:xs) ew) -> do setAuxilary (InO fd de xs ew)
                                           return (Just x)
                 

queueStore :: (MonadState CPU m, MonadIO m) => RegisterName -> Address -> m ()
queueStore reg addr = do (CPU _ _ _ _ aux) <- get
                         case aux of
                           Nil -> fail "Need In-order auxilary data"
                           (InO fd de em ew) -> 
                             setAuxilary (InO fd de em 
                                          (ew ++ [(reg,addr)]))

getStore :: (MonadState CPU m, MonadIO m) => m (Maybe (RegisterName,Address))
getStore = do (CPU _ _ _ _ aux) <- get
              case aux of
               Nil -> fail "Need In-order auxilary data"
               (InO _ _ _ []) -> return Nothing
               (InO fd de em (x:xs)) -> do setAuxilary (InO fd de em xs)
                                           return (Just x)

type Counters = Map String Integer

emptyCounters :: Counters
emptyCounters = (Map.insert "StallUntil" 0 
                 (Map.insert "Cycles" 0 Map.empty))

-- Return 0 when counter name doesn't exist
getCounter :: (MonadState CPU m, MonadIO m) => String -> m Integer
getCounter id = do (CPU _ _ _ cs _) <- get
                   if Map.member id cs then return $ cs Map.! id else return 0

setCounter :: (MonadState CPU m, MonadIO m) => String -> Integer -> m ()
setCounter id cnt = do (CPU m rs dbg cs aux) <- get
                       put $ CPU m rs dbg (Map.insert id cnt cs) aux

stallForN :: (MonadState CPU m, MonadIO m) => Integer -> m ()
stallForN n = do cyc <- currentCycle
                 setCounter "StallUntil" (cyc + n)
              

stallCycle :: (MonadState CPU m, MonadIO m) => m Integer
stallCycle = getCounter "StallUntil"

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

-- <<<<<<< HEAD
--data Memory = Mem CacheHierarchy (Map Address Word32)
--  deriving Show
-- =======
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
                       put $ (CPU (Mem (cache m) (setBound (layout m) seg bnd) (mem m)) rs dbg cs aux)

getBoundM ::  (MonadState CPU m, MonadIO m) => Segment -> m (Address, Address)
getBoundM seg = do (CPU m rs dbg cs aux) <- get
                   return (getBound (layout m) seg)

data Memory = Mem { cache  :: CacheHierarchy,
                       layout :: MemLayout,
                       mem    :: Map Address Word32} deriving (Show)

type Address = Word32

type WordAddress = Address

type ByteAddress = Address

emptyMem :: Hierarchy -> Memory
emptyMem h = Mem (buildHierarchy h) initMemLayout Map.empty


wordAddress :: ByteAddress -> WordAddress
wordAddress addr = addr `div` 4

getMemWord :: (MonadState CPU m, MonadIO m) => WordAddress -> m Word32
getMemWord addr = do (CPU (Mem c l m) _ _ _ _) <- get
                     if Map.member addr m 
                       then return (m Map.! addr) else return 0

setMemWord :: (MonadState CPU m, MonadIO m) => WordAddress -> Word32 -> m ()
setMemWord addr val = do (CPU (Mem c l m) rs dbg cs aux) <- get
                         put $ (CPU (Mem c l (Map.insert addr val m)) rs dbg cs aux)


readMem :: (MonadState CPU m, MonadIO m) => Address -> m Word32
readMem byteAddr = do val <- getMemWord (wordAddress byteAddr)
                      lat <- loadCache byteAddr
                      updateCache byteAddr
                      return val
    
writeMem :: (MonadState CPU m, MonadIO m) => Address -> Word32 -> m ()
writeMem byteAddr val = do updateCache byteAddr
                           setMemWord (wordAddress byteAddr) val

-- CacheLevel Size Block-Size Associativity Latency
data CacheLevel = CacheLevel Integer Integer Integer Integer
  deriving Show

type Hierarchy = [CacheLevel]

type CacheHierarchy = [Cache]

buildHierarchy :: Hierarchy -> CacheHierarchy
buildHierarchy []     = []
buildHierarchy (x:xs) = (Cache x Map.empty) : (buildHierarchy xs)

standardCache :: Hierarchy
standardCache = [(CacheLevel 32768 32 1 10), (CacheLevel 4194304 128 2 100)]

-- Only determine if a block would be in the cache, simplify implementation by 
-- not actually storing data there
type Line = (Word32,Bool)

type Set = [Line]

-- Cache CacheLevel Latency (Map Index [(Tag,Dirty,Word32)])
data Cache = Cache CacheLevel (Map Word32 Set)
  deriving Show

updateCache :: (MonadState CPU m, MonadIO m) => Address -> m ()
updateCache addr = do (CPU (Mem c l m) rs dbg cs aux) <- get
                      put (CPU 
                           (Mem 
                            (foldr 
                             (\c cs -> (insertInCache c addr) : cs) [] c) 
                            l m) rs dbg cs aux)
    
insertInCache :: Cache -> Address -> Cache
insertInCache c@(Cache l m) addr = 
  if Map.member idx m then 
    let set = m Map.! idx in
    (Cache l (Map.insert idx (insertInSet set tag) m))
  else (Cache l (Map.insert idx (insertInSet [] tag) m))
  where
    idx = getIndex addr l
    tag = getTag addr l

-- Doing lru replacement by just removing the first thing in the list
insertInSet :: Set -> Word32 -> Set
insertInSet s tag = if any (\(t,_) -> (tag == t)) s then s 
                        else aux s tag
  where
    aux []     t = [(t,False)]
    aux (l:ls) t = ls ++ [(t,False)]

latency :: CacheLevel -> Integer
latency (CacheLevel _ _ _ l) = l

-- Simulate a load to the cache hierarchy and return the latency for this load
loadCache :: (MonadState CPU m, MonadIO m) => Address -> m Integer
loadCache addr = do (CPU (Mem c _ _) _ _ _ _) <- get
                    return $ foldr f 1000 c
  where
    f cache@(Cache lev ls) i = if inCache addr cache
                         then min (latency lev) i else i
                                                       
inCache :: Address -> Cache -> Bool
inCache addr (Cache lev st) = (Map.member idx st) && 
                              (any (\(t,_) -> t == tag) lns)
  where
    idx = getIndex addr lev
    tag = getTag addr lev
    lns = st Map.! idx

offsetBits :: CacheLevel -> Int
offsetBits (CacheLevel _ b _ _) = round $ logBase 2 (fromInteger b)

indexBits :: CacheLevel -> Int
indexBits (CacheLevel si bi ai _) = round $ logBase 2 (s / (b * a))
  where
    s = fromInteger si
    b = fromInteger bi
    a = fromInteger ai

-- Using 32 bit addresses
tagBits :: CacheLevel -> Int
tagBits c = 32 - (indexBits c) - (offsetBits c)

getTag :: Address -> CacheLevel -> Word32
getTag addr c = shiftR addr (32 - (tagBits c))

getIndex :: Address -> CacheLevel -> Word32
getIndex addr c = a .&. mask
  where
    a    = shiftR addr (offsetBits c)
    mask = complement $ shiftL (complement (0 :: Word32)) (indexBits c)
    
getOffset :: Address -> CacheLevel -> Word32
getOffset addr (CacheLevel _ b _ _) = addr `mod` (fromIntegral b)

