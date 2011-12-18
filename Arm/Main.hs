{-#OPTIONS -XFlexibleContexts #-}
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Assembler
import Arm
import Loader
import Program
import Debugger
import Memory (standardCache)
import Stage (inOrder)

----------------------
-- Run a program
----------------------

run :: String -> IO ()
run fileName
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> Main.run prog pipe h
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
                                  
runFromFileN :: String -> Pipeline -> Hierarchy -> IO ()
runFromFileN fileName pipe h
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> Main.runN prog pipe h
         Right err
           -> putStrLn err
              
runN :: Program -> Pipeline -> Hierarchy -> IO ()
runN program pipe h
  = do cpu <- runProgramN program pipe h 100
       putStrLn $ show cpu
                               
checkN :: String -> IO Bool
checkN file = do p <- asmFile file
                 case p of
                   Left prog -> do (CPU (Mem c1 l1 m1) r1 _ _ _) <- runProgramN prog simplePipe [] 100
                                   (CPU (Mem c2 l2 m2) r2 _ _ _) <- runProgramN prog inOrder [] 100
                                   return (m1 == m2 && (Map.insert R15 0 r1) == (Map.insert R15 0 r2))
                   Right err -> do putStrLn err
                                   return False

checkProgram :: Program -> IO Bool
checkProgram prog = do liftIO $ putStrLn (show prog ++ "\n\n")
                       cp1@(CPU (Mem _ _ m1) r1 _ _ _) <- runProgramN prog simplePipe [] 100
                       cp2@(CPU (Mem _ _ m2) r2 _ _ _) <- runProgramN prog inOrder [] 100
                       if (m1 == m2 && (Map.insert R15 0 r1) == (Map.insert R15 0 r2)) then
                         return True else
                         do liftIO $ putStrLn ((show cp1) ++ "\n\n")
                            liftIO $ putStrLn ((show cp2) ++ "\n\n")
                            return False

runStepN :: (MonadState CPU m, MonadIO m) => Pipeline -> Integer -> m ()
runStepN p n = do singleStep p
                  r   <- isRunning
                  i   <- instrsExecuted
                  if (r == False) || (i >= n) then return () else (runStepN p n)

runProgramN :: Program -> Pipeline -> Hierarchy -> Integer -> IO CPU
runProgramN program pipe h n = do cpu <- (execStateT (loadProgram program) 
                                          (CPU (emptyMem h) emptyRegs (D False)
                                           emptyCounters emptyAux)) 
                                  cpu' <- execStateT startRunning cpu
                                  cpu'' <- execStateT (runStepN pipe n) cpu'
                                  -- Need to clear out any remaining memory operations
                                  cpu''' <- execStateT (memWrite) cpu''
                                  execStateT (memRead) cpu'''
                                  

prop_exec :: Program -> Property
prop_exec p = monadicIO $ do b <- Test.QuickCheck.Monadic.run $ checkProgram p
                             Test.QuickCheck.Monadic.assert $ b