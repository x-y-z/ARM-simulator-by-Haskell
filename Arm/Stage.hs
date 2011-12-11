{-#OPTIONS -XRankNTypes -XFlexibleContexts -XImpredicativeTypes#-}
module Stage
where       
  
import Data.Word  
  
import Control.Monad.State  
  
import CPU  
import qualified Decoder
import ExecutionUnit
import Instruction
import RegisterName

type Pipeline = [Stage]

type Stage = (MonadState CPU m, MonadIO m) => m ()

inOrder :: Pipeline
inOrder = [memWrite,memRead,execute,decode,fetch]

setFD :: (MonadState CPU m, MonadIO m) => Word32 -> m ()
setFD i = do (CPU _ _ _ a) <- get
             case a of
                  Nil -> fail "Need In-Order auxilary data"
                  (InO fd de em ew) -> setAuxilary (InO (i : fd) de em ew)

fetch :: Stage
fetch = do pc <- getReg R15
           nextCycle
           cyc <- currentCycle
           sta <- stallCycle
           if cyc >= sta then do opcode <- readMem pc
                                 if opcode == 0 then stopRunning 
                                   else do setFD opcode
                                           setReg R15 (pc + 4) 
             else return ()

getFD :: (MonadState CPU m, MonadIO m) => m (Maybe Word32)
getFD = do (CPU _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO fd de em ew) -> case fd of
               [] -> return Nothing
               (x : xs) -> do setAuxilary (InO xs de em ew)
                              return (Just x)

setDE :: (MonadState CPU m, MonadIO m) => Instruction -> m ()
setDE i = do (CPU _ _ _ a) <- get
             case a of
               Nil -> fail "Need In-Order auxilary data"
               (InO fd de em ew) -> setAuxilary (InO fd (i : de) em ew)

getDE :: (MonadState CPU m, MonadIO m) => m (Maybe Instruction)
getDE = do (CPU _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO fd de em ew) -> case de of
               [] -> return Nothing
               (x : xs) -> do setAuxilary (InO fd xs em ew)
                              return (Just x)

decode :: Stage
decode = do opcode <- getFD
            case opcode of
              Nothing -> return ()
              Just op -> case (Decoder.decode op) of
                Nothing -> fail "Unable to decode"
                Just i -> do setDE i

execute :: Stage
execute = do instr <- getDE
             case instr of
               Nothing -> return ()
               Just i -> do evalInO i
             
-- We need a recursive call to this function to handle multiple reads
memRead :: Stage
memRead = do ld <- getLoad
             case ld of
               Nothing -> return ()
               Just (r,a) -> do v <- readMem a
                                setReg r v
                                memRead

-- We need a recursive call to this function to handle multiple writes
memWrite :: Stage
memWrite = do st <- getStore
              case st of
                Nothing -> return ()
                Just (r,a) -> do v <- getReg r
                                 writeMem a v
                                 memWrite

simplePipe :: Pipeline
simplePipe = [singleStage]

-- Executes pipeline in a single step
singleStage :: Stage
singleStage = do pc <- getReg R15
                 nextCycle
                 opcode <- readMem pc
                 nextCycle
                 case (Decoder.decode opcode) of
                   Nothing -> do fail ("ERROR: can't decode instruction")
                   Just i -> do setReg R15 (pc + 4)
                                nextCycle
                                eval i