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
main = do _ <- runTestTT $ TestList [ cacheTests, test_programs, testEval ]
          quickCheck prop_address_bits
          quickCheck prop_dm_cache_insert
          quickCheck prop_a_cache_insert
          quickCheck prop_align_mem_access
          quickCheck prop_eval_eq
          quickCheck prop_exec
          return ()

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