{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-#OPTIONS -XMultiParamTypeClasses -XTypeSynonymInstances -XFlexibleContexts -XFunctionalDependencies -XFlexibleInstances -XUndecidableInstances#-}
module Memory (
               ----functions
               getMemLayout_, setMemLayout_, getBound_, setBound_,
               getMemData_, setMemData_, getMemWord_, setMemWord_, align_, 
               getCacheH_, hasCache_, inCache_, latency_, insertInCache_,
               setCacheH_, getTag_, getIndex_, getOffset_, indexBits_,
               offsetBits_,
               ----data structures
               Address, Memory(Mem, cache, mem, layout), CacheHierarchy, 
               MemLayout, MemData, Cache(Cache), CacheLevel(CacheLevel), 
               CacheData, Set, Line, CacheStruct, 
               Segment(CodeS, DataS, StackS, HeapS), 
               Bound(Bound, lowerB, upperB), Hierarchy, emptyMem, 
               standardCache,cacheTests,prop_address_bits,
               prop_dm_cache_insert,prop_a_cache_insert,
               prop_align_mem_access,
               ----dummies
               MemNoCache (cacheNC, layoutNC, memNC), CacheHDummy, CacheDummy, 
               CacheLDummy, CacheDDummy, StructDummy, SetDummy, LineDummy,
               ----classes
               CMemory
              )
where


-----------------------
-- system library
-----------------------

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits
import Data.Word

import Test.QuickCheck
import Test.HUnit

---------------------------
-- classes
---------------------------

class (Ord addr, Show datum) => CMemData mem addr datum
      |mem->addr, mem->datum, addr->mem, addr->datum where
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
      stdL1ACache_ :: cl
      latency_ :: cl -> Integer
      offsetBits_ :: cl -> Int
      indexBits_ :: cl -> Int
      tagBits_ :: cl -> Int
      getTag_ :: addr -> cl -> addr
      getIndex_ :: addr -> cl -> addr
      getOffset_ :: addr -> cl -> addr

class (CCacheData cdata idx set line tag valid, CCacheLevel cl struct addr) =>
       CCache cache cl cdata addr idx set line tag valid struct
       |cache->cl, cache->cdata, cache->idx, cache->set, cache->line, 
        cache->tag, cache->valid, cache->struct, cache->addr where
        
      insertInCache_ :: cache -> addr -> cache
      inCache_ :: cache -> addr -> Bool

class (CCache cache cl cdata addr idx set line tag valid struct) => 
      CCacheHierarchy ch cache cl cdata addr idx set line tag valid struct
      | ch->cache, ch->cl where
      buildHierarchy :: [cl] -> ch
--      standardCache :: [cl]
      hasCache_ :: ch -> Bool 
      hasCache_ _ = False

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



------------------------
-- instances
------------------------
data Memory = Mem { cache :: CacheHierarchy,
                    layout :: MemLayout,
                    mem :: MemData} deriving Show

type Address = Word32
type WordAddress = Address

type MemData = Map WordAddress Word32

emptyMem :: Hierarchy -> Memory
emptyMem h = Mem (buildHierarchy h) initMemLayout_ Map.empty


instance CMemData MemData WordAddress Word32 where
         emptyMem_ = Map.empty
         align_ addr = (addr `div` 4) * 4
         getMemWord_ mem' addr = if Map.member addr mem'
                                 then mem' Map.! addr
                                 else 0
         setMemWord_ mem' addr datum = Map.insert addr datum mem'

data Segment = CodeS | DataS | StackS | HeapS deriving (Ord, Eq, Show)
data Bound = Bound {lowerB :: Address, 
                    upperB :: Address} deriving Show

type MemLayout = Map Segment Bound

instance CMemLayout MemLayout Segment Bound where
         initMemLayout_ = Map.fromList [(CodeS, Bound 0 0),(DataS, Bound 0 0),
                                        (StackS, Bound 0 0),(HeapS, Bound 0 0)]
         getBound_ mly seg = case (Map.lookup seg mly) of
                                  Just bnd -> bnd
                                  Nothing  -> error "segment fault"         
         setBound_ mly seg bnd = case (Map.lookup seg mly) of
                                      Just _  -> Map.insert seg bnd mly
                                      Nothing -> error "segment fault"

-- (Tag value, Valid bit (currently unused))
type Line = (Word32, Bool)

type Set = [Line]

-- Maps from an index to a set (multiple cache lines)
type CacheData = Map Word32 Set

-- CacheLevel Size Block-Size Associativity Latency
type CacheStruct = (Integer, Integer, Integer, Integer)

data CacheLevel = CacheLevel CacheStruct
  deriving Show
           
data Cache = Cache CacheLevel CacheData deriving Show

-- Describes a cache configuration to be turned into a CacheHierarchy
type Hierarchy = [CacheLevel]

-- Actual cache hierarchy used by the CPU
type CacheHierarchy = [Cache]

instance CLine Line Word32 Bool   
    
instance CSet Set Line Word32 Bool where
         insertInSet_ s tag = if any (\(t, _) -> tag == t) s then s
                                 else aux s tag
            where aux []     t = [(t, False)]
                  aux (_:ls) t = ls ++ [(t,False)] 
                  -- this causes LRU replacement
                  
instance CCacheData CacheData Word32 Set Line Word32 Bool where
         insertInCacheData_ cdata idx tag 
           = if Map.member idx cdata 
             then let set = cdata Map.! idx
                  in Map.insert idx (insertInSet_ set tag) cdata
             else Map.insert idx (insertInSet_ [] tag) cdata
         inCacheData_ idx tag cdata = (Map.member idx cdata) && 
                                      (any (\(t, _) -> t == tag) lns)
           where lns = cdata Map.! idx -- Lazy evaluation win
         emptyCacheData_ = Map.empty

instance CCacheLevel CacheLevel CacheStruct Word32 where
         -- Standard L1 (Direct-mapped), L2 (Associative), and L2 caches
         stdL1Cache_ = CacheLevel (32768,32,1,10)
         stdL2Cache_ = CacheLevel (4194304,128,2,100)
         stdL1ACache_ = CacheLevel (32768,32,4,16)
         
         latency_ (CacheLevel (_,_,_,l)) = l
         
         -- Functions for computing tag, index, and offset from an address
         -- address ==> [ tag | index | offset ]
         offsetBits_ (CacheLevel (_,b,_,_)) = round $ logBase 2 (fromInteger b)
         indexBits_ (CacheLevel (si,bi,ai,_)) = round $ logBase 2 (s / (b * a))
           where s = fromInteger si
                 b = fromInteger bi
                 a = fromInteger ai 
         tagBits_ c = 32 - (indexBits_ c) - (offsetBits_ c)
         
         getTag_ addr c = shiftR addr (32 - (tagBits_ c))
         getIndex_ addr c = a Data.Bits..&. mask
            where a = shiftR addr (offsetBits_ c)
                  mask = complement $ shiftL (complement (0::Word32)) 
                                             (indexBits_ c)
         getOffset_ addr  (CacheLevel (_,b,_,_)) = addr `mod` (fromIntegral b)
          


instance CCache Cache CacheLevel CacheData 
                Word32 Word32 Set Line Word32 Bool CacheStruct where
         insertInCache_ (Cache l m) addr 
           = (Cache l (insertInCacheData_ m idx tag))
           where idx = getIndex_ addr l
                 tag = getTag_ addr l
         inCache_ (Cache lev st) addr = inCacheData_ idx tag st
           where idx = getIndex_ addr lev
                 tag = getTag_ addr lev


instance CCacheHierarchy CacheHierarchy Cache CacheLevel CacheData 
                         Word32 Word32 Set
         Line Word32 Bool CacheStruct where
         buildHierarchy []     = []
         -- Builds a CacheHierarchy from a list
         -- of CacheLevels (cache descriptions)
         buildHierarchy (x:xs) = (Cache x emptyCacheData_) 
                                 : (buildHierarchy xs)
         hasCache_ _ = True

standardCache :: Hierarchy
standardCache = [stdL1Cache_, stdL2Cache_]

instance CMemory Memory CacheHierarchy MemLayout MemData 
                 Cache CacheLevel CacheData 
                 Word32 Word32 Set Line 
                 Word32 Bool CacheStruct Word32 Segment Bound where
         getMemLayout_ mem' = layout mem'
         setMemLayout_ mem' lyt = mem' {layout = lyt}
         getMemData_ memory = mem memory
         setMemData_ memory mdata = memory {mem = mdata}
         getCacheH_ mem' = cache mem'
         setCacheH_ mem' ch = mem' {cache = ch}


------------------------
-- Dummy
------------------------

data CacheHDummy
instance Show CacheHDummy where
         show _ = "No Cache"

data CacheDummy
data CacheLDummy
data CacheDDummy
data StructDummy
data SetDummy
data LineDummy

instance CLine LineDummy Word32 Bool

instance CSet SetDummy LineDummy Word32 Bool where
         insertInSet_ = undefined

instance CCacheData CacheDDummy Word32 SetDummy LineDummy Word32 Bool where
         emptyCacheData_ = undefined
         inCacheData_ = undefined
         insertInCacheData_ = undefined

instance CCacheLevel CacheLDummy StructDummy Word32 where
         getOffset_ = undefined
         getIndex_ = undefined
         getTag_ = undefined
         tagBits_ = undefined
         indexBits_ = undefined
         offsetBits_ = undefined
         latency_ = undefined
         stdL1ACache_ = undefined
         stdL2Cache_ = undefined
         stdL1Cache_ = undefined

instance CCache CacheDummy CacheLDummy CacheDDummy Word32 Word32 SetDummy LineDummy
         Word32 Bool StructDummy where
         inCache_ = undefined
         insertInCache_ = undefined


instance CCacheHierarchy CacheHDummy CacheDummy CacheLDummy CacheDDummy 
         Word32 Word32 SetDummy LineDummy Word32 Bool StructDummy where
         buildHierarchy = undefined
--         buildCache = undefined

data MemNoCache = MemNC { cacheNC :: CacheHDummy,
                          layoutNC :: MemLayout,
                          memNC :: MemData} deriving Show

instance CMemory MemNoCache CacheHDummy MemLayout MemData 
                 CacheDummy CacheLDummy CacheDDummy 
                 Word32 Word32 SetDummy LineDummy 
                 Word32 Bool StructDummy Word32 Segment Bound where
         getMemLayout_ mem' = layoutNC mem'
         setMemLayout_ mem' lyt = mem' {layoutNC = lyt}
         getMemData_ memory = memNC memory
         setMemData_ memory mdata = memory {memNC = mdata}
         getCacheH_ mem' = cacheNC mem'
         setCacheH_ mem' ch = mem' {cacheNC = ch}

------------------------
-- test
------------------------


testCL :: CacheLevel
testCL = stdL1Cache_

testCLA :: CacheLevel
testCLA = stdL1ACache_

testDMCache :: Cache
testDMCache = (Cache testCL emptyCacheData_)

testACache :: Cache
testACache = (Cache testCLA emptyCacheData_)

prop_dm_cache_insert :: Address -> Property
prop_dm_cache_insert a = property $ inCache_ (insertInCache_ testDMCache a) a

prop_a_cache_insert :: Address -> Property
prop_a_cache_insert a = property $ inCache_ (insertInCache_ testACache a) a

cacheTests :: Test
cacheTests = TestList [ testOffset, testIndex, testTag ]

testOffset :: Test
testOffset = TestList [ getOffset_ 0x00000001 testCL ~?= 1, 
                        getOffset_ 0x00000020 testCL ~?= 0 ]

testIndex :: Test
testIndex = TestList [ getIndex_ 0x00000020 testCL ~?= 1,
                       getIndex_ 0x0000001F testCL ~?= 0,
                       getIndex_ 0x00008000 testCL ~?= 0 ]
            
testTag :: Test
testTag = TestList [ getTag_ 0x00008000 testCL ~?= 1 ]

-- Check that the split address sums to the original address
prop_address_bits :: Address -> Property
prop_address_bits a = 
  property $ (shiftL (getTag_ a c) ts) + 
  (shiftL (getIndex_ a c) is) + getOffset_ a c == a
  where
    c = testCL
    ts = (indexBits_ c) + is
    is = offsetBits_ c


prop_align_mem_access :: Address -> Word32 -> Property
prop_align_mem_access a d =
     property $ (getMemWord_ (setMemWord_ emptyMem_ a' d) a') == d
     where a' = align_ a