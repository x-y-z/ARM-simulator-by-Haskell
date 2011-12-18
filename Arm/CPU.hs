{-#OPTIONS -XMultiParamTypeClasses -XTypeSynonymInstances -XFlexibleContexts -XFunctionalDependencies -XFlexibleInstances -XUndecidableInstances#-}

module CPU 
where

-----------------------
--  system library
-----------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits
import Data.Word
import Control.Monad.State
import Test.QuickCheck hiding ((.&.))
import Test.HUnit

-----------------------
-- user library
-----------------------

import Instruction
import RegisterName
import Memory
import Register

----------------------------
-- Code
----------------------------

class CDebug dbg where
      isDebug_  ::  dbg -> Bool
      setDbg_ ::  dbg -> dbg
      clrDbg_ ::  dbg -> dbg



-- | for each parameter of CPU, we make it a class, providing primitive 
-- operations. For CPU, we will define it as below, bring the definition
-- of monad m at the same time.


class CCounter cnt name val|cnt->name, cnt->val where
      getCounter_ :: cnt -> name -> val
      setCounter_ :: cnt -> name -> val -> cnt
      emptyCounters_ :: cnt


class (CDebug dbg, CMemory memory ch memlayout memdata cache cl cdata addr idx 
       set line tag valid struct datum seg bnd, CRegisters reg rname rval,  
       CCounter cnt cname cval) => 
       CCPU cpu memory reg dbg cnt ch memlayout memdata cache cl cdata addr idx 
       set line tag valid struct datum seg bnd rname rval cname cval
       |cpu->memory, cpu->reg, cpu->dbg, cpu->cnt, cnt->cpu where
-- cpu
       getRegFile_ :: cpu -> reg
       setRegFile_ :: cpu -> reg -> cpu
       getMem_ :: cpu -> memory
       setMem_ :: cpu -> memory -> cpu
       getDebug_ :: cpu -> dbg
       setDebug_ :: cpu -> dbg -> cpu
       getCnt_ :: cpu -> cnt
       setCnt_ :: cpu -> cnt -> cpu
       getAux_ :: cpu -> Auxilary         -- ad hoc function
       setAux_ :: cpu -> Auxilary -> cpu  -- ad hoc function
-- registers
       cpsrGet :: (MonadState cpu m, MonadIO m) => Int -> m rval
       cpsrSet :: (MonadState cpu m, MonadIO m) => Int -> m ()
-- cache
       loadCache :: (MonadState cpu m, MonadIO m) => addr -> m Integer
       updateCache :: (MonadState cpu m, MonadIO m) => addr -> m ()
-- memory 
       setBoundM :: (MonadState cpu m, MonadIO m) => 
                    seg -> (addr, addr) -> m ()
       getBoundM :: (MonadState cpu m, MonadIO m) => seg -> m (addr, addr)
-- auxilary
       setAuxilary :: (MonadState cpu m, MonadIO m) => Auxilary -> m ()
       setAuxilary aux = do c <- get
                            let c' = setAux_ c aux
                            put c'
       flushPipeline :: (MonadState cpu m, MonadIO m) => m ()
--       emptyAux :: Auxilary
       queueLoad :: (MonadState cpu m, MonadIO m) => rname -> addr -> m ()
       queueStore :: (MonadState cpu m, MonadIO m) => Word32 -> addr -> m ()
       getLoad :: (MonadState cpu m, MonadIO m) => m (Maybe (rname, addr))
       getStore :: (MonadState cpu m, MonadIO m) => m (Maybe (Word32, addr))

-- counters
       stallForN :: (MonadState cpu m, MonadIO m) => cval -> m ()
       stallCycle :: (MonadState cpu m, MonadIO m) => m cval
       currentCycle :: (MonadState cpu m, MonadIO m) => m cval
       nextCycle :: (MonadState cpu m, MonadIO m) => m ()
       advanceCycle :: (MonadState cpu m, MonadIO m) => Integer -> m ()
       startRunning :: (MonadState cpu m, MonadIO m) => m ()
       stopRunning :: (MonadState cpu m, MonadIO m) => m ()
       isRunning :: (MonadState cpu m, MonadIO m) => m Bool
       instrsExecuted :: (MonadState cpu m, MonadIO m) => m cval
       executedInstr :: (MonadState cpu m, MonadIO m) => m ()


-- counters
       emptyCounters :: cnt
       emptyCounters = emptyCounters_
       getCounter :: (MonadState cpu m, MonadIO m) => cname -> m cval
       getCounter n = do c <- get
                         let counter = getCnt_ c
                         return $ getCounter_ counter n
       setCounter :: (MonadState cpu m, MonadIO m) => cname -> cval -> m ()
       setCounter n v = do c <- get
                           let counter = getCnt_ c
                           let counter' = setCounter_ counter n v
                           let c' = setCnt_ c counter'
                           put c'
       incrCounter :: (MonadState cpu m, MonadIO m, Num cval) => cname -> m ()
       incrCounter n = do v <- getCounter n
                          setCounter n (v + 1)
-- memory
       setBound :: (MonadState cpu m, MonadIO m) => seg -> bnd -> m ()
       setBound s b = do cpu <- get
                         let m = getMem_ cpu
                         let lyt = getMemLayout_ m
                         let lyt' = setBound_ lyt s b
                         let m' = setMemLayout_ m lyt'
                         put $ setMem_ cpu m'
                         return ()
       getBound :: (MonadState cpu m, MonadIO m) => seg -> m bnd
       getBound s = do cpu <- get
                       let m = getMem_ cpu
                       let lyt = getMemLayout_ m
                       return $ getBound_ lyt s
       getMemWord :: (MonadState cpu m, MonadIO m) => addr -> m datum
       getMemWord addr = do cpu <- get
                            let md = getMemData_ $ getMem_ cpu
                            return $ getMemWord_ md addr
       setMemWord :: (MonadState cpu m, MonadIO m) => addr -> datum -> m ()
       setMemWord a d = do cpu <- get
                           let m = getMem_ cpu
                           let md = getMemData_ m
                           let md' = setMemWord_ md a d
                           let m' = setMemData_ m md'
                           let cpu' = setMem_ cpu m'
                           put cpu'
       readMem :: (MonadState cpu m, MonadIO m) => addr -> m datum
       readMem addr = do val <- getMemWord (align_ addr)
                         yesCache <- hasCache
                         if yesCache 
                         then do lat <- loadCache addr
                                 updateCache addr
                                 return val
                         else return val
                         
       writeMem :: (MonadState cpu m, MonadIO m) => addr -> datum -> m ()
       writeMem a d = do updateCache a
                         setMemWord (align_ a) d
-- cache
       hasCache :: (MonadState cpu m, MonadIO m) => m Bool
       hasCache = do cpu <- get
                     let mem = getMem_ cpu
                     let ch = getCacheH_ mem
                     return $ hasCache_ ch
                           
-- debug
       isDebug :: (MonadState cpu m, MonadIO m) => m Bool
       isDebug = do c <- get
                    let d = getDebug_ c
                    return $ isDebug_ d

-- registers
       getReg :: (MonadState cpu m, MonadIO m) => rname -> m rval
       getReg regname = do c <- get
                           let rs = getRegFile_ c
                           return $ getReg_ rs regname
       setReg :: (MonadState cpu m, MonadIO m) => rname -> rval -> m ()
       setReg name val = do c <- get
                            let rs = getRegFile_ c
                            let rs' = setReg_ rs name val
                            put $ setRegFile_ c rs'
       cpsrGetN, cpsrGetZ, cpsrGetC, cpsrGetV 
               ::(MonadState cpu m, MonadIO m) => m rval
       cpsrGetN = cpsrGet 31
       cpsrGetZ = cpsrGet 30
       cpsrGetC = cpsrGet 29
       cpsrGetV = cpsrGet 28

       cpsrSetN, cpsrSetZ, cpsrSetC, cpsrSetV 
               :: (MonadState cpu m, MonadIO m) => m ()
       cpsrSetN = cpsrSet 31
       cpsrSetZ = cpsrSet 30
       cpsrSetC = cpsrSet 29
       cpsrSetV = cpsrSet 28

       showCPSRFlags :: (MonadState cpu m, MonadIO m) => m ()
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

data Debug = D Bool deriving Show



instance CDebug Debug  where
     isDebug_ (D a) = a 
     setDbg_ (D _) = D True
     clrDbg_ (D _) = D False



type Counters = Map String Integer


instance CCounter Counters String Integer where
         emptyCounters_ = Map.fromList [("StallUntil", 0),("Cycles", 0)]
         getCounter_ cnt id = if Map.member id cnt
                              then cnt Map.! id
                              else 0
         setCounter_ cnt id val = Map.insert id val cnt

-- in CPU class, loadCache and updateCache needed



data Auxilary = 
    Nil
  | InO {fd :: [Word32], 
         de :: [Instruction], 
         em :: [(RegisterName,Address)], 
         ew :: [(Word32,Address)]}
  deriving Show

emptyAux :: Auxilary
emptyAux = InO [] [] [] []


data CPU = CPU { mems :: Memory,
                 reg :: Registers,
                 dbg :: Debug,
                 cnt :: Counters,
                 aux :: Auxilary } deriving Show


instance CCPU CPU Memory Registers Debug Counters 
         CacheHierarchy MemLayout MemData
         Cache CacheLevel CacheData Word32 Word32 Set Line Word32 Bool
         CacheStruct Word32 Segment Bound RegisterName Word32 String Integer
         where
       getRegFile_ c = reg c
       setRegFile_ c r = c {reg = r}
       getMem_ c = mems c
       setMem_ c m = c {mems = m}
       getDebug_ c = dbg c
       setDebug_ c d = c {dbg = d}
       getCnt_ c = cnt c
       setCnt_ c cn = c {cnt = cn}
       getAux_ c = aux c         -- ad hoc function
       setAux_ c a = c {aux = a} -- ad hoc function
-- registers
       cpsrGet bit = do cpsr <- getReg CPSR
                        if cpsr `testBit` bit then return 1 else return 0
       cpsrSet bit = do cpsr <- getReg CPSR
                        let cpsr' = cpsr `setBit` bit
                        setReg CPSR cpsr'
-- cache
       loadCache addr = do cpu <- get
                           let c = cache (mems cpu)
                           return $ foldr f 1000 c
         where f cache@(Cache lev ls) i = if inCache_ cache addr
                                          then min (latency_ lev) i else i
       updateCache addr = do cpu <- get
                             let m = mems cpu
                             let c = getCacheH_ m
                             let c' = (foldr 
                                       (\c cs -> (insertInCache_ c addr) : cs)
                                       [] 
                                       c) 
                             let m' = setCacheH_ m c'
                             put cpu {mems = m'} 
-- memory 
       setBoundM seg bnd = do cpu <- get
                              let m = mems cpu
                              let b = getMemLayout_ m
                              let b' = setBound_ b seg 
                                                 (Bound (fst bnd) (snd bnd))
                              let m' = setMemLayout_ m b'
                              put cpu {mems = m'}

       getBoundM seg = do cpu <- get
                          let b = getMemLayout_ $ mems cpu
                          let bnd = getBound_ b seg
                          return (lowerB bnd, upperB bnd)
-- auxilary
       flushPipeline = setAuxilary emptyAux
--       emptyAux :: Auxilary
       queueLoad reg addr = do (CPU _ _ _ _ aux) <- get
                               case aux of
                                 Nil -> fail "Need In-order auxilary data"
                                 (InO fd de em ew) -> 
                                   setAuxilary (InO fd 
                                                    de 
                                                    (em ++ [(reg,addr)]) 
                                                    ew)
       queueStore val addr = do (CPU _ _ _ _ aux) <- get
                                case aux of
                                  Nil -> fail "Need In-order auxilary data"
                                  (InO fd de em ew) -> 
                                    setAuxilary (InO fd 
                                                     de 
                                                     em 
                                                     (ew ++ [(val,addr)]))
       getLoad = do (CPU _ _ _ _ aux) <- get
                    case aux of
                      Nil -> fail "Need In-order auxilary data"
                      (InO _ _ [] _) -> return Nothing
                      (InO fd de (x:xs) ew) -> do setAuxilary (InO fd de xs ew)
                                                  return (Just x)
       getStore = do (CPU _ _ _ _ aux) <- get
                     case aux of
                       Nil -> fail "Need In-order auxilary data"
                       (InO _ _ _ []) -> return Nothing
                       (InO fd de em (x:xs)) -> do setAuxilary (InO fd 
                                                                    de 
                                                                    em 
                                                                    xs)
                                                   return (Just x)

-- counters
       stallForN n = do cyc <- currentCycle
                        setCounter "StallUntil" (cyc + n)
       stallCycle = getCounter "StallUntil"
       currentCycle = getCounter "Cycles"
       nextCycle = do cyc <- getCounter "Cycles"
                      setCounter "Cycles" (cyc + 1)
       advanceCycle n = do cyc <- getCounter "Cycles"
                           setCounter "Cycles" (cyc + n)
       startRunning = setCounter "Running" 1
       stopRunning = setCounter "Running" 0

       isRunning = do r <- getCounter "Running"
                      return $ r == 1
       instrsExecuted = getCounter "ExecutedInstructions"
       
       executedInstr = do is <- getCounter "ExecutedInstructions"
                          setCounter "ExecutedInstructions" (is + 1)


-- ========test code ====================
testDMCache :: Cache
testDMCache = (Cache (CacheLevel (32768,32,1,10)) Map.empty)

testACache :: Cache
testACache = (Cache (CacheLevel (32768,32,4,16)) Map.empty)

prop_dm_cache_insert :: Address -> Property
prop_dm_cache_insert a = property $ inCache_ (insertInCache_ testDMCache a) a

prop_a_cache_insert :: Address -> Property
prop_a_cache_insert a = property $ inCache_ (insertInCache_ testACache a) a


testCL :: CacheLevel
testCL = CacheLevel (32768,32,1,10)

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

-- Check that 
prop_address_bits :: Address -> Property
prop_address_bits a = 
  property $ (shiftL (getTag_ a c) ts) + 
  (shiftL (getIndex_ a c) is) + getOffset_ a c == a
  where
    c = testCL
    ts = (indexBits_ c) + is
    is = offsetBits_ c

-- ===============test above=========