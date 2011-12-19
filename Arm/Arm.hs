{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-#OPTIONS -XFlexibleContexts #-}
module Arm
where

import Data.Map ()  
import qualified Data.Map as Map
  
import Control.Monad.State

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Assembler
import CPU
import Memory
import Register (emptyRegs)
import Loader
import Program
import RegisterName
import Stage

-- Divides execution into steps (e.g. one turn for each stage in the pipeline)
runStep :: (MonadState CPU m, MonadIO m) => Pipeline -> m ()
runStep p = do singleStep p
               r <- isRunning
               if r == False then return () else (runStep p)


-- Runs each stage of the pipeline
singleStep :: (MonadState CPU m, MonadIO m) => Pipeline -> m ()
singleStep []       = return ()
singleStep (s : ss) = do r <- isRunning
                         if r == False then return () else 
                           do s 
                              singleStep ss
                              return ()

-- Loads the program into memory, sets the "Running" counter, 
-- and starts execution
runProgram :: Program -> Pipeline -> Hierarchy -> IO CPU
runProgram program pipe h = do cpu <- (execStateT (loadProgram program) 
                                       (CPU (emptyMem h) emptyRegs (D False)
                                        emptyCounters_ emptyAux)) 
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
           -> Arm.run prog pipe h
         Right err
           -> putStrLn err
              
-- Runs a program file with the in-order pipeline (the one we built) 
-- and the original (single stage execution) to check that 
-- the registers and memory are equal after execution
check :: String -> IO Bool
check file = do p <- asmFile file
                case p of
                  Left prog -> do (CPU (Mem _ _ m1) r1 _ _ _) 
                                       <- runProgram prog simplePipe []
                                  (CPU (Mem _ _ m2) r2 _ _ _) 
                                       <- runProgram prog inOrder []
                                  return (m1 == m2 && 
                                          (Map.insert R15 0 r1) == 
                                          (Map.insert R15 0 r2))
                  Right err -> do putStrLn err
                                  return False
                                  

{- 
The following functions are similar to the above except they
only execute for N instructions.  These are here to allow us to 
test our execution model against randomly generated assembly programs.
Obviously, these random programs will contain infinite loops, so 
we need to cut off their execution.  However, their outputs (for checking),
should be the same after the same number of instructions are executed.
-}

runFromFileN :: String -> Pipeline -> Hierarchy -> IO ()
runFromFileN fileName pipe h
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> runN prog pipe h
         Right err
           -> putStrLn err
              
runN :: Program -> Pipeline -> Hierarchy -> IO ()
runN program pipe h
  = do cpu <- runProgramN program pipe h 100
       putStrLn $ show cpu
                               
checkN :: String -> IO Bool
checkN file = do p <- asmFile file
                 case p of
                   Left prog -> do (CPU (Mem _ _ m1) r1 _ _ _) <- 
                                     runProgramN prog simplePipe [] 100
                                   (CPU (Mem _ _ m2) r2 _ _ _) <- 
                                     runProgramN prog inOrder [] 100
                                   return (m1 == m2 && 
                                           (Map.insert R15 0 r1) == 
                                           (Map.insert R15 0 r2))
                   Right err -> do putStrLn err
                                   return False

checkProgram :: Program -> IO Bool
checkProgram prog = do cp1@(CPU (Mem _ _ m1) r1 _ _ _) <- 
                         runProgramN prog simplePipe [] 100
                       cp2@(CPU (Mem _ _ m2) r2 _ _ _) <- 
                         runProgramN prog inOrder [] 100
                       fd1 <- (evalStateT (getCounter "DecodeFailures") cp1)
                       fd2 <- (evalStateT (getCounter "DecodeFailures") cp2)
                       tm1 <- (evalStateT (getCounter "TooMany") cp1)
                       tm2 <- (evalStateT (getCounter "TooMany") cp2)
                       if (tm1 ==1 || tm2 == 1 || fd1 > 10 || fd2 > 10 || 
                           (m1 == m2 && (Map.insert R15 0 r1) == 
                           (Map.insert R15 0 r2))) then
                         return True else
                         do liftIO $ putStrLn ((show cp1) ++ "\n\n")
                            liftIO $ putStrLn ((show cp2) ++ "\n\n")
                            return False

runStepN :: (MonadState CPU m, MonadIO m) => Pipeline -> Integer -> m ()
runStepN p n = do singleStep p
                  r   <- isRunning
                  i   <- instrsExecuted
                  c <- getCounter "Cycles"
                  if c > 11000 then do setCounter "TooMany" 1 
                                       return ()
                    else
                    if (r == False) || (i >= n) then return () 
                    else (runStepN p n)

runProgramN :: Program -> Pipeline -> Hierarchy -> Integer -> IO CPU
runProgramN program pipe h n = do cpu <- (execStateT (loadProgram program) 
                                          (CPU (emptyMem h) emptyRegs (D False)
                                           emptyCounters_ emptyAux)) 
                                  cpu' <- execStateT startRunning cpu
                                  cpu'' <- execStateT (runStepN pipe n) cpu'
                                  -- Need to clear out any 
                                  -- remaining memory operations
                                  cpu''' <- execStateT (memWrite) cpu''
                                  execStateT (memRead) cpu'''
                                  

prop_exec :: Program -> Property
prop_exec p = monadicIO $ do b <- Test.QuickCheck.Monadic.run $ checkProgram p
                             Test.QuickCheck.Monadic.assert $ b
                             
test_programs :: Test
test_programs = TestList [ (TestCase $ testProg "p1.arm"), 
                           (TestCase $ testProg "p3.arm"),
                           (TestCase $ testProgN "p4.arm"),
                           (TestCase $ testProgN "p5.arm") ]
                
testProg :: String -> Assertion                
testProg p = do b <- check p
                assertBool ("Failed to run " ++ p) b
                
testProgN :: String -> Assertion                
testProgN p = do b <- checkN p
                 assertBool ("Failed to run " ++ p) b