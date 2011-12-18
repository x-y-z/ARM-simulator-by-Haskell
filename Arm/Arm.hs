module Arm
where

import Assembler
import qualified Main
import Loader
import Program
import Debugger
import CPU (standardCache)
import Stage (inOrder)

run :: String -> IO ()
run fileName
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> Main.run prog inOrder standardCache 
         Right err
           -> putStrLn err

----------------------------------------------------------------------
-- Debug a program.
----------------------------------------------------------------------
dbg
  :: String     -- program's file name
  -> IO ()

dbg fileName
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> debug prog
         Right err
           -> putStrLn err

