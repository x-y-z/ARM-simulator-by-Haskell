{-#OPTIONS -XMultiParamTypeClasses -XTypeSynonymInstances -XFlexibleContexts -XFunctionalDependencies -XFlexibleInstances -XUndecidableInstances#-}

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

class (Ord name, Show val) => CRegisters reg name val|reg->name, reg->val where
      emptyRegs_ :: reg
      getReg_ :: reg -> name -> val
      setReg_ :: reg -> name -> val -> reg
      cpsrGet_ :: reg -> Int -> val
      cpsrSet_ ::reg -> Int -> reg

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

class (Ord addr, Show datum) => CMemData mem addr datum|mem->addr, mem->datum where
      emptyMem_ ::mem
      align_ :: addr -> addr
      getMemWord_ :: mem -> addr -> datum
      setMemWord_ :: mem -> addr -> datum -> mem
      


class (Ord seg, Eq seg, Show seg) => CMemLayout lyt seg bnd|lyt->seg, lyt->bnd where
      initMemLayout_ :: lyt
      getBound_ :: lyt -> seg -> bnd
      setBound_ :: lyt -> seg -> bnd -> lyt


class CLine line tag valid|line->tag, line->valid
      
class (CLine line tag valid) => CSet set line tag valid
      |set->line, set->tag, set->valid where
      insertInSet_ :: set -> tag -> set
      
class (CSet set line tag valid) => CCacheData cdata idx set line tag valid
      |cdata->set, cdata->line, cdata->tag, cdata->valid, cdata->idx where

      insertInCacheData_ :: cdata -> idx -> tag -> cdata
      inCacheData_ :: idx -> tag -> cdata -> Bool
      emptyCacheData_ :: cdata

class CCacheLevel cl struct addr|cl->struct, cl->addr where
      stdL1Cache_ :: cl
      stdL2Cache_ :: cl
      latency_ :: cl -> Integer
      offsetBits_ :: cl -> Int
      indexBits_ :: cl -> Int
      tagBits_ :: cl -> Int
      getTag_ :: addr -> cl -> addr
      getIndex_ :: addr -> cl -> addr
      getOffset_ :: addr -> cl -> addr

class (CCacheData cdata idx set line tag valid, CCacheLevel cl struct addr) =>
       CCache cache cl cdata addr idx set line tag valid struct
       |cache->cl, cache->cdata, cache->idx, cache->set, cache->line, cache->tag, 
       cache->valid, cache->struct, cache->addr where
        
      insertInCache_ :: cache -> addr -> cache
      inCache_ :: cache -> addr -> Bool

class (CCache cache cl cdata addr idx set line tag valid struct) => 
      CCacheHierarchy ch cache cl cdata addr idx set line tag valid struct
      | ch->cache, ch->cl where
      buildHierarchy :: [cl] -> ch
      standardCache :: [cl]
      hasCache :: ch -> Bool 
      hasCache _ = False

class (CCacheHierarchy ch cache cl cdata addr idx set line tag valid struct, 
       CMemData memdata addr datum, CMemLayout memlayout seg bnd) =>
       CMemory memory ch memlayout memdata cache cl cdata addr idx 
               set line tag valid struct datum seg bnd
       |memory->ch, memory->memlayout, memory->memdata where
       getMemLayout_ :: memory -> memlayout
       setMemLayout_ :: memory -> memlayout -> memory
       getMemData_ :: memory -> memdata
       setMemData_ :: memory -> memdata -> memory
       getCacheH_ :: memory -> ch
       setCacheH_ :: memory -> ch -> memory

class CCounter cnt name val|cnt->name, cnt->val where
      getCounter_ :: cnt -> name -> val
      setCounter_ :: cnt -> name -> val -> cnt
      emptyCounters_ :: cnt



-- temporary CPU, revision needed
{-class (CDebug dbg, CRegisters rs rn rd, 
       CMemData md maddr mdata, CMemLayout ml mseg mbnd)=>
     CCPU cpu dbg rs md ml rn rd maddr mdata mseg mbnd
     |cpu -> dbg, cpu -> rs, cpu -> md, cpu -> ml where
     getAuxilary :: cpu -> Auxilary
      
data CPU = CPU MemData MemLayout Debug Registers Auxilary

instance CCPU CPU Debug Registers MemData MemLayout RegisterName Word32 Word32 Word32 Segment Bound where

 cpu->ch, cpu->memlayout, 
        cpu->memdata, cpu->cache, cpu->cl, cpu->cdata, cpu->addr, cpu->idx,
        cpu->set, cpu->line, cpu->tag, cpu->valid, cpu->struct, cpu->datum, 
        cpu->seg, cpu->bnd, cpu->rname, cpu->rval, cpu->cname, cpu->cval,

-}

class (CDebug dbg, CMemory memory ch memlayout memdata cache cl cdata addr idx 
       set line tag valid struct datum seg bnd, CRegisters reg rname rval,  
       CCounter cnt cname cval, MonadState cpu m, MonadIO m) => 
       CCPU cpu memory reg dbg cnt ch memlayout memdata cache cl cdata addr idx 
       set line tag valid struct datum seg bnd rname rval cname cval m
       |cpu->memory, cpu->reg, cpu->dbg, cpu->cnt, cpu->m where
-- cpu
       getRegFile :: cpu -> reg
       setRegFile :: cpu -> reg -> cpu
       getMem :: cpu -> memory
       setMem :: cpu -> memory -> cpu
       getDbg :: cpu -> dbg
       setDbg :: cpu -> dbg -> cpu
       getCnt :: cpu -> cnt
       setCnt :: cpu -> cnt -> cpu
       getAux :: cpu -> Auxilary         -- ad hoc function
       setAux :: cpu -> Auxilary -> cpu  -- ad hoc function
-- registers
       cpsrGet :: Int -> m rval
       cpsrSet :: Int -> m ()
-- memory
       setBound :: seg -> bnd -> m ()
       setBound s b = do cpu <- get
                         let m = getMem cpu
                         let lyt = getMemLayout_ m
                         let lyt' = setBound_ lyt s b
                         let m' = setMemLayout_ m lyt'
                         put $ setMem cpu m'
                         return ()
       getBound :: seg -> m bnd
       getBound s = do cpu <- get
                       let m = getMem cpu
                       let lyt = getMemLayout_ m
                       return $ getBound_ lyt s
       setBoundM :: seg -> (addr, addr) -> m ()
       getBoundM :: seg -> m (addr, addr)
       getMemWord :: addr -> m datum
       getMemWord addr = do cpu <- get
                            let md = getMemData_ $ getMem cpu
                            return $ getMemWord_ md addr
       setMemWord :: addr -> datum -> m ()
       setMemWord a d = do cpu <- get
                           let m = getMem cpu
                           let md = getMemData_ m
                           let md' = setMemWord_ md a d
                           let m' = setMemData_ m md'
                           let cpu' = setMem cpu m'
                           put cpu'
       readMem :: addr -> m datum
--       readMem addr = 
       writeMem :: addr -> datum -> m ()
-- cache
       loadCache :: addr -> m Integer
       updateCache :: addr -> m ()
                           


-- registers
       getReg :: rname -> m rval
       getReg regname = do c <- get
                           let rs = getRegFile c
                           return $ getReg_ rs regname
       setReg :: rname -> rval -> m ()
       setReg name val = do c <- get
                            let rs = getRegFile c
                            let rs' = setReg_ rs name val
                            put $ setRegFile c rs'
       cpsrGetN, cpsrGetZ, cpsrGetC, cpsrGetV :: m rval
       cpsrGetN = cpsrGet 31
       cpsrGetZ = cpsrGet 30
       cpsrGetC = cpsrGet 29
       cpsrGetV = cpsrGet 28
       cpsrSetN, cpsrSetZ, cpsrSetC, cpsrSetV :: m ()
       cpsrSetN = cpsrSet 31
       cpsrSetZ = cpsrSet 30
       cpsrSetC = cpsrSet 29
       cpsrSetV = cpsrSet 28
       showCPSRFlags :: m ()
       showCPSRFlags = do n <- cpsrGetN
                          z <- cpsrGetZ
                          c <- cpsrGetC
                          v <- cpsrGetV
                          liftIO $ putStr ("N=" ++ show n ++ 
                                           " Z=" ++ show z ++ 
                                           " C=" ++ show c ++ 
                                           " V=" ++ show v)


--  instance of all stuffs

{-data CPU = CPU Memory Registers Debug Counters Auxilary
     deriving Show-}

data Debug = D Bool

type Registers = Map RegisterName Word32


instance CDebug Debug  where
     isDebug_ (D a) = a 
     setDbg_ (D _) = D True
     clrDbg_ (D _) = D False


instance CRegisters Registers RegisterName Word32  where
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

instance CMemData MemData WordAddress Word32 where
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

instance CMemLayout MemLayout Segment Bound where
         initMemLayout_ = Map.fromList [(CodeS, Bound 0 0), (DataS, Bound 0 0), 
                                       (StackS, Bound 0 0), (HeapS, Bound 0 0)]
         getBound_ mly seg = case (Map.lookup seg mly) of
                                  Just bnd -> bnd
                                  Nothing  -> error "segment fault"         
         setBound_ mly seg bnd = case (Map.lookup seg mly) of
                                      Just _ -> Map.insert seg bnd mly
                                      Nothing          -> error "segment fault"

type Line = (Word32, Bool)
type Set = [Line]
type CacheData = Map Word32 Set
-- CacheLevel Size Block-Size Associativity Latency
type CacheStruct = (Integer, Integer, Integer, Integer)
data CacheLevel = CacheLevel CacheStruct
  deriving Show
data Cache = Cache CacheLevel CacheData deriving Show

type Hierarchy = [CacheLevel]
type CacheHierarchy = [Cache]



instance CLine Line Word32 Bool   
    
instance CSet Set Line Word32 Bool where
         insertInSet_ s tag = if any (\(t, _) -> tag == t) s then s
                                 else aux s tag
            where aux []     t = [(t, False)]
                  aux (l:ls) t = ls ++ [(t,False)] -- this causes LRU replacement      



instance CCacheData CacheData Word32 Set Line Word32 Bool where
         insertInCacheData_ cdata idx tag 
           = if Map.member idx cdata 
             then let set = cdata Map.! idx
                  in Map.insert idx (insertInSet_ set tag) cdata
             else Map.insert idx (insertInSet_ [] tag) cdata
         inCacheData_ idx tag cdata = (Map.member idx cdata) && 
                                      (any (\(t, _) -> t == tag) lns)
           where lns = cdata Map.! idx  -- is this OK? If idx is not in cdata
         emptyCacheData_ = Map.empty


instance CCacheLevel CacheLevel CacheStruct Word32 where
         stdL1Cache_ = CacheLevel (32768,32,1,10)
         stdL2Cache_ = CacheLevel (4194304,128,2,100)
         latency_ (CacheLevel (_,_,_,l)) = l
         offsetBits_ (CacheLevel (_,b,_,_)) = round $ logBase 2 (fromInteger b)
         indexBits_ (CacheLevel (si,bi,ai,_)) = round $ logBase 2 (s / (b * a))
           where s = fromInteger si
                 b = fromInteger bi
                 a = fromInteger ai 
         tagBits_ c = 32 - (indexBits_ c) - (offsetBits_ c)
         getTag_ addr c = shiftR addr (32 - (tagBits_ c))
         getIndex_ addr c = a .&. mask
            where a = shiftR addr (offsetBits_ c)
                  mask = complement $ shiftL (complement (0::Word32)) (indexBits_ c)
         getOffset_ addr  (CacheLevel (_,b,_,_)) = addr `mod` (fromIntegral b)
          


instance CCache Cache CacheLevel CacheData Word32 Word32 Set Line Word32 Bool CacheStruct where
         insertInCache_ c@(Cache l m) addr = (Cache l (insertInCacheData_ m idx tag))
           where idx = getIndex_ addr l
                 tag = getTag_ addr l
         inCache_ (Cache lev st) addr = inCacheData_ idx tag st
           where idx = getIndex_ addr lev
                 tag = getTag_ addr lev

{-class (CCache cache cl cdata addr idx set line tag valid struct) => 
      CCacheHierarchy ch cache cl cdata addr idx set line tag valid struct
      | ch->cache, ch->cl where
      buildHierarchy :: [cl] -> ch
      standardCache :: [cl]-}
instance CCacheHierarchy CacheHierarchy Cache CacheLevel CacheData Word32 Word32 Set
         Line Word32 Bool CacheStruct where
         buildHierarchy []     = []
         buildHierarchy (x:xs) = (Cache x emptyCacheData_): (buildHierarchy xs)

         standardCache = [stdL1Cache_, stdL2Cache_]
         hasCache _ = True


type Counters = Map String Integer

{-class CCounter cnt name val where
      getCounter_ :: cnt -> name -> val
      setCounter_ :: cnt -> name -> val -> cnt-}
instance CCounter Counters String Integer where
         emptyCounters_ = Map.fromList [("StallUntil", 0),("Cycles", 0)]
         getCounter_ cnt id = if Map.member id cnt
                              then cnt Map.! id
                              else 0
         setCounter_ cnt id val = Map.insert id val cnt

-- in CPU class, loadCache and updateCache needed
data Memory = Mem { cache :: CacheHierarchy,
                    layout :: MemLayout,
                    mem :: MemData} deriving Show

{-class (CCacheHierarchy ch cache cl cdata addr idx set line tag valid struct, 
       CMemData memdata addr datum, CMemLayout memlayout seg bnd) =>
       CMemory memory ch memlayout memdata cache cl cdata addr idx 
               set line tag valid struct datum seg bnd
       |memory->ch, memory->memlayout, memory->memdata-}
instance CMemory Memory CacheHierarchy MemLayout MemData Cache CacheLevel CacheData 
         Word32 Word32 Set Line Word32 Bool CacheStruct Word32 Segment Bound where
         getMemLayout_ mem = layout mem
         setMemLayout_ mem lyt = mem {layout = lyt}
         getMemData_ memory = mem memory
         setMemData_ memory mdata = memory {mem = mdata}
         getCacheH_ mem = cache mem
         setCacheH_ mem ch = mem {cache = ch}


data Auxilary = 
    Nil
  | InO {fd :: [Word32], de :: [Instruction], em :: [(RegisterName,Address)], ew :: [(RegisterName,Address)]}
  deriving Show



data CPU = CPU Memory Registers Debug Counters Auxilary
{- MonadState cpu m, MonadIO m) => 
       CCPU cpu memory reg dbg cnt ch memlayout memdata cache cl cdata addr idx 
       set line tag valid struct datum seg bnd rname rval cname cval m-}
{-
instance  (MonadState CPU m, MonadIO m) => 
          CCPU CPU Memory Registers Debug Counters CacheHierarchy MemLayout MemData
              Cache CacheLevel CacheData Word32 Word32 Set Line Word32 Bool 
              CacheStruct Word32 Segment Bound RegisterName Word32 String Integer m
          where
-}

{-
type CacheDummy = Int

instance CCacheHierarchy CacheDummy undefined undefined undefined undefined undefined 
         undefined undefined undefined undefined undefined where
         buildHierarchy = undefined
         standardCache = undefined
         hasCache _ = True
-}