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
setFD i = do (CPU _ _ _ _ a) <- get
             case a of
                  Nil -> fail "Need In-Order auxilary data"
                  (InO fd de em ew) -> setAuxilary (InO (i : fd) de em ew)

fetch :: Stage
fetch = do pc <- getReg R15
           nextCycle
           cyc <- currentCycle
           sta <- stallCycle
           df  <- failedDecodes
           if cyc > sta then do opcode <- readMem pc
                                if opcode == 0 then 
                                  do setReg R15 (pc + 4)
                                     decodeFail
                                     if df > 10 then
                                       do stopRunning
                                          return ()
                                       else return ()
                                  else do setFD opcode
                                          setReg R15 (pc + 4) 
             else return ()


getFD :: (MonadState CPU m, MonadIO m) => m (Maybe Word32)
getFD = do (CPU _ _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO fd de em ew) -> case fd of
               [] -> return Nothing
               (x : xs) -> do setAuxilary (InO xs de em ew)
                              return (Just x)

setDE :: (MonadState CPU m, MonadIO m) => Instruction -> m ()
setDE i = do (CPU _ _ _ _ a) <- get
             case a of
               Nil -> fail "Need In-Order auxilary data"
               (InO fd de em ew) -> setAuxilary (InO fd (i : de) em ew)


getDE :: (MonadState CPU m, MonadIO m) => m (Maybe Instruction)
getDE = do (CPU _ _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO fd de em ew) -> case de of
               [] -> return Nothing
               (x : xs) -> do setAuxilary (InO fd xs em ew)
                              return (Just x)

decode :: Stage
decode = do cyc <- currentCycle
            sta <- stallCycle
            if cyc >= sta then
              do opcode <- getFD
                 case opcode of
                   Nothing -> return ()
                   Just op -> case (Decoder.decode op) of
                     Nothing -> fail "Unable to decode"
                     Just i -> do setDE i 
              else return ()

execute :: Stage
execute = do cyc <- currentCycle
             sta <- stallCycle
             if cyc >= sta then
               do instr <- getDE
                  case instr of
                    Nothing -> return ()
                    Just i -> do evalInO i
                                 executedInstr
               else return ()
             
-- We need a recursive call to this function to handle multiple reads
memRead :: Stage
memRead = do ld <- getLoad
             case ld of
               Nothing -> return ()
               Just (r,a) -> do l <- loadCache a
                                if l > 1 then
                                  do stallForN l
                                     v <- readMem a
                                     setReg r v
                                  else
                                  do v <- readMem a
                                     setReg r v

-- We need a recursive call to this function to handle multiple writes
memWrite :: Stage
memWrite = do st <- getStore
              case st of
                Nothing -> return ()
                Just (v,a) -> do writeMem a v

simplePipe :: Pipeline
simplePipe = [singleStage]

-- Executes pipeline in a single step
singleStage :: Stage
singleStage = do pc <- getReg R15
                 nextCycle
                 df <- failedDecodes
                 opcode <- readMem pc
                 if opcode == 0 then do setReg R15 (pc + 4)
                                        decodeFail
                                        if df > 10 then
                                          do stopRunning
                                             return () 
                                          else return ()
                                        else
                   do nextCycle
                      case (Decoder.decode opcode) of
                        Nothing -> do fail ("ERROR: can't decode instruction")
                        Just i -> do setReg R15 (pc + 4)
                                     nextCycle
                                     eval i
                                     executedInstr