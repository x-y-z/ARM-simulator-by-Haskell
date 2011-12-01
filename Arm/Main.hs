import Control.Monad.State

import CPU
import Loader
import Program
import Stage

runStep :: State CPU ()
runStep = do singleStep inOrder
             cyc <- currentCycle
             r   <- isRunning
             if r || cyc > 1000 then return () else runStep

singleStep :: Pipeline -> State CPU ()
singleStep []       = return ()
singleStep (s : ss) = do r <- isRunning
                         if r then return () else do {s; singleStep ss}

runProgram :: Program -> CPU
runProgram program = execState runStep
                     (execState startRunning 
                     (execState (loadProgram program) 
                      (CPU emptyMem emptyRegs emptyCounters emptyAux)))

run :: Program -> IO ()
run program
  = do putStrLn $ show $ runProgram program