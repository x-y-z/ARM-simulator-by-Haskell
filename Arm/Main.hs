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
           -> Arm.run prog inOrder standardCache 
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


