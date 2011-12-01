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

type Stage = State CPU ()

inOrder :: Pipeline
inOrder = [fetch,decode,execute]

setFD :: Word32 -> State CPU ()
setFD i = do (CPU _ _ _ a) <- get
             case a of
                  Nil -> fail "Need In-Order auxilary data"
                  (InO fd de) -> setAuxilary (InO (i : fd) de)

fetch :: Stage
fetch = do pc <- getReg R15
           opcode <- readMem pc
           if opcode == 0 then stopRunning 
             else do setFD opcode
                     setReg R15 (pc + 4)

getFD :: State CPU Word32
getFD = do (CPU _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO fd de) -> case fd of
               [] -> fail "No instructions to decode"
               (x : xs) -> do setAuxilary (InO xs de)
                              return x

setDE :: Instruction -> State CPU ()
setDE i = do (CPU _ _ _ a) <- get
             case a of
               Nil -> fail "Need In-Order auxilary data"
               (InO fd de) -> setAuxilary (InO fd (i : de))

getDE :: State CPU Instruction
getDE = do (CPU _ _ _ a) <- get
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