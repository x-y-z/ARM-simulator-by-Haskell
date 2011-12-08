{-#OPTIONS -XFlexibleContexts #-}
import Control.Monad.State

import CPU
import Loader
import Program
import Stage

runStep :: (MonadState CPU m, MonadIO m) => m ()
runStep = do singleStep inOrder
             cyc <- currentCycle
             r   <- isRunning
             if r || cyc > 1000 then return () else runStep

singleStep :: (MonadState CPU m, MonadIO m) => Pipeline -> m ()
singleStep []       = return ()
singleStep (s : ss) = do r <- isRunning
                         if r then return () else do {s; singleStep ss}



{-
run' :: (MonadState CPU m, MonadIO m) => m ()

run' = do singleStep
          run'

singleStep :: (MonadState CPU m, MonadIO m) => m ()
singleStep
  = do pc <- getReg R15
       opcode <- readMem pc
       let instr = decode opcode
       case instr of
         Nothing
           -> do fail ("ERROR: can't decode instruction " ++ (formatHex 8 '0' "" opcode)
                           ++ " at adddress " ++ show pc ++ " (dec)")
         Just instr'
           -> do setReg R15 (pc + 4)
                 eval instr'
-}

runProgram :: Program -> IO CPU
runProgram program = do cpu <- (execStateT (loadProgram program) 
                                (CPU emptyMem emptyRegs False emptyCounters emptyAux)) 
                        cpu' <- execStateT startRunning cpu
                        execStateT runStep cpu'
                    

run :: Program -> IO ()
run program
  = do cpu <- runProgram program
       putStrLn $ show cpu