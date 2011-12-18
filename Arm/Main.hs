{-#OPTIONS -XFlexibleContexts #-}
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Test.HUnit

import Assembler
import CPU
import Loader
import Program
import RegisterName
import Stage

runStep :: (MonadState CPU m, MonadIO m) => Pipeline -> m ()
runStep p = do singleStep p
               r <- isRunning
               if r == False then return () else (runStep p)


singleStep :: (MonadState CPU m, MonadIO m) => Pipeline -> m ()
singleStep []       = return ()
singleStep (s : ss) = do r <- isRunning
                         if r == False then return () else 
                           do s 
                              --cpu <- get
                              --liftIO $ putStrLn $ show cpu
                              singleStep ss
                              return ()

runProgram :: Program -> Pipeline -> Hierarchy -> IO CPU
runProgram program pipe h = do cpu <- (execStateT (loadProgram program) 
                                       (CPU (emptyMem h) emptyRegs (D False)
                                        emptyCounters emptyAux)) 
                               cpu' <- execStateT startRunning cpu
                               execStateT (runStep pipe) cpu'
                    

run :: Program -> Pipeline -> Hierarchy -> IO ()
run program pipe h
  = do cpu <- runProgram program pipe h
       putStrLn $ show cpu
       

runFromFile :: String -> Pipeline -> Hierarchy -> IO ()
runFromFile fileName pipe h
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> run prog pipe h
         Right err
           -> putStrLn err
              
check :: String -> IO Bool
check file = do p <- asmFile file
                case p of
                  Left prog -> do (CPU (Mem c1 l1 m1) r1 _ _ _) <- runProgram prog simplePipe []
                                  (CPU (Mem c2 l2 m2) r2 _ _ _) <- runProgram prog inOrder []
                                  return (m1 == m2 && (Map.insert R15 0 r1) == (Map.insert R15 0 r2))
                  Right err -> do putStrLn err
                                  return False
                                  

{-
<<<<<<< HEAD
testInO :: Test
testInO = TestList [ ]
                               
main :: IO ()
main = do _ <- runTestTT $ TestList [testInO]
          return () 
=======
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


runProgram :: Program -> IO CPU
runProgram program = do cpu <- (execStateT (loadProgram program) 
                                (CPU emptyMem emptyRegs False emptyCounters emptyAux)) 
                        cpu' <- execStateT startRunning cpu
                        execStateT runStep cpu'
                    

run :: Program -> IO ()
run program
  = do cpu <- runProgram program
       putStrLn $ show cpu
>>>>>>> addDebug-}
