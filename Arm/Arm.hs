module Arm
where

import Assembler
import qualified Main
import Loader
import Program

run :: String -> IO ()
run fileName
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> Main.run prog
         Right err
           -> putStrLn err
{-
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
           -> Arm.Debugger.dbg prog
         Right err
           -> putStrLn err

-}