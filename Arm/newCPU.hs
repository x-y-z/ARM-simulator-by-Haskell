{-#OPTIONS -XMultiParamTypeClasses -XTypeSynonymInstances -XFlexibleContexts#-}

module CPU where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits
import Data.Word

import Control.Monad.State

import Instruction
import RegisterName

class CDebug dbg where
      isDebug_  ::  dbg -> Bool
      setDbg_ ::  dbg -> dbg
      clrDbg_ ::  dbg -> dbg



-- | for each parameter of CPU, we make it a class, providing primitive 
-- operations. For CPU, we will define it as below, bring the definition
-- of monad m at the same time.

class (Ord name, Show val) => CRegisters map name val where
      emptyRegs_ :: map name val
      getReg_ :: map name val -> name -> val
      setReg_ :: map name val -> name -> val ->  map name val
      cpsrGet_ :: map name val -> Int -> val
      cpsrSet_ :: map name val -> Int -> map name val

      cpsrGetN_, cpsrGetZ_, cpsrGetC_, cpsrGetV_ :: map name val -> val
      cpsrSetN_, cpsrSetZ_, cpsrSetC_, cpsrSetV_:: map name val -> map name val
      
      cpsrGetN_ rs = cpsrGet_ rs 31
      cpsrSetN_ rs = cpsrSet_ rs 31

      cpsrGetZ_ rs = cpsrGet_ rs 30
      cpsrSetZ_ rs = cpsrSet_ rs 30

      cpsrGetC_ rs = cpsrGet_ rs 29
      cpsrSetC_ rs = cpsrSet_ rs 29

      cpsrGetV_ rs = cpsrGet_ rs 28
      cpsrSetV_ rs = cpsrSet_ rs 28
      
      showCPSRFlags_ :: map name val -> IO ()           
      showCPSRFlags_ rs = let n = cpsrGetN_ rs
                              z = cpsrGetZ_ rs
                              c = cpsrGetC_ rs
                              v = cpsrGetV_ rs
                          in putStr ("N=" ++ show n ++ 
                                    " Z=" ++ show z ++ 
                                    " C=" ++ show c ++ 
                                    " V=" ++ show v)

class (Ord addr, Show datum) => CMemData map addr datum where
      emptyMem_ :: map addr datum
      align_ :: addr -> addr
      getMemWord_ :: map addr datum -> addr -> datum
      setMemWord_ :: map addr datum -> addr -> datum -> map addr datum
      


class (Ord seg, Eq seg, Show seg) => CMemLayout map seg bound where
      initMemLayout_ :: map seg bound
      getBound_ :: map seg bound -> seg -> bound
      setBound_ :: map seg bound -> seg -> bound -> map seg bound




--  instance of all stuffs

{-data CPU = CPU Memory Registers Debug Counters Auxilary
     deriving Show-}

data Debug = D Bool

type Registers = Map RegisterName Word32


instance CDebug Debug  where
     isDebug_ (D a) = a 
     setDbg_ (D _) = D True
     clrDbg_ (D _) = D False

instance CRegisters Map RegisterName Word32  where
         emptyRegs_ = Map.fromList[(R0,0), (R1,0), (R2,0),
                                  (R3,0), (R4,0), (R5,0),
                                  (R6,0), (R7,0), (R8,0),
                                  (R9,0), (R10,0),(R11,0),
                                  (R12,0),(R13,0),(R14,0),
                                  (R15,0),(CPSR,0),(PC,0)]
         getReg_ rs id = rs Map.! id

         setReg_ rs id val = Map.insert id val rs
         cpsrGet_ rs bit = if cpsr `testBit` bit then 1 else 0
                 where cpsr = getReg_ rs CPSR
         cpsrSet_ rs bit = let cpsr = getReg_ rs CPSR
                               cpsr' = cpsr `setBit` bit
                           in setReg_ rs CPSR cpsr'

type Address = Word32
type WordAddress = Address
type ByteAddress = Address

type MemData = Map WordAddress Word32

instance CMemData Map WordAddress Word32 where
         emptyMem_ = Map.empty
         align_ addr = (addr `div` 4) * 4
         getMemWord_ mem addr = if Map.member addr mem
                               then mem Map.! addr
                               else 0
         setMemWord_ mem addr datum = Map.insert addr datum mem

data Segment = CodeS | DataS | StackS | HeapS deriving (Ord, Eq, Show)
data Bound = Bound {lowerB :: Address, 
                    upperB :: Address} deriving Show

type MemLayout = Map Segment Bound

instance CMemLayout Map Segment Bound where
         initMemLayout_ = Map.fromList [(CodeS, Bound 0 0), (DataS, Bound 0 0), 
                                       (StackS, Bound 0 0), (HeapS, Bound 0 0)]
         getBound_ mly seg = case (Map.lookup seg mly) of
                                  Just bnd -> bnd
                                  Nothing  -> error "segment fault"         
         setBound_ mly seg bnd = case (Map.lookup seg mly) of
                                      Just _ -> Map.insert seg bnd mly
                                      Nothing          -> error "segment fault"



-- in CPU class, loadCache and updateCache needed
data Memory = Mem { layout :: MemLayout,
                    mem :: MemData} deriving Show

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




-- =======CPU==========
data CPU = CPU Memory Registers Debug 

setReg :: (MonadState CPU m, MonadIO m) => RegisterName -> Word32 -> m ()
setReg id val = do (CPU mem rs dbg) <- get
                   put (CPU mem (setReg_ rs id val) dbg)

