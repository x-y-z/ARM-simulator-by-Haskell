{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
import Assembler
import Arm
import Debugger
import ExecutionUnit (testEval,prop_eval_eq)
import Memory (standardCache,cacheTests,
               prop_address_bits,prop_dm_cache_insert,
               prop_a_cache_insert,prop_align_mem_access)
import Stage (inOrder)

import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do putStrLn $ "Choose your option(1. run programs, 2. debug programs, 3. run"
                   ++ "tests):"
          opt <- getChar
          _ <- getChar
          case opt of 
               '3' -> do _ <- runTestTT $ TestList [ cacheTests, 
                                                     test_programs, 
                                                     testEval ]
                         quickCheck prop_address_bits
                         quickCheck prop_dm_cache_insert
                         quickCheck prop_a_cache_insert
                         quickCheck prop_align_mem_access
                         quickCheck prop_eval_eq
                         quickCheck prop_exec
                         return ()
               '1' -> do putStrLn "Please give the name of the file:"
                         file <- getLine
                         Main.run file
               '2' -> do putStrLn "Please give the name of the file:"
                         file <- getLine
                         dbg file
               _ -> error "Wrong option, program ends"

----------------------
-- Run a program
----------------------

run :: String -> IO ()
run fileName
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> Arm.run prog inOrder standardCache
         Right err
           -> putStrLn err

----------------------------------------------------------------------
-- Debug a program.
----------------------------------------------------------------------
dbg
  :: String -- program's file name
  -> IO ()

dbg fileName
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> debug prog
         Right err
           -> putStrLn err