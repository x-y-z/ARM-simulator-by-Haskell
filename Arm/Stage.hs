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
inOrder = [fetch,decode,execute]

setFD :: (MonadState CPU m, MonadIO m) => Word32 -> m ()
setFD i = do (CPU _ _ _ _ a) <- get
             case a of
                  Nil -> fail "Need In-Order auxilary data"
                  (InO fd de) -> setAuxilary (InO (i : fd) de)

fetch :: Stage
fetch = do pc <- getReg R15
           opcode <- readMem pc
           if opcode == 0 then stopRunning 
             else do setFD opcode
                     setReg R15 (pc + 4)

getFD :: (MonadState CPU m, MonadIO m) => m Word32
getFD = do (CPU _ _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO fd de) -> case fd of
               [] -> fail "No instructions to decode"
               (x : xs) -> do setAuxilary (InO xs de)
                              return x

setDE :: (MonadState CPU m, MonadIO m) => Instruction -> m ()
setDE i = do (CPU _ _ _ _ a) <- get
             case a of
               Nil -> fail "Need In-Order auxilary data"
               (InO fd de) -> setAuxilary (InO fd (i : de))

getDE :: (MonadState CPU m, MonadIO m) => m Instruction
getDE = do (CPU _ _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO fd de) -> case de of
               [] -> fail "No instructions to execute"
               (x : xs) -> do setAuxilary (InO fd xs)
                              return x

decode :: Stage
decode = do opcode <- getFD
            case (Decoder.decode opcode) of
              Nothing -> fail ("ERROR: can't decode " ++ 
                                     "instruction ")
              Just i -> do setDE i

execute :: Stage
execute = do instr <- getDE
             nextCycle
             eval instr
             

memRead :: Stage
memRead = undefined

memWrite :: Stage
memWrite = undefined