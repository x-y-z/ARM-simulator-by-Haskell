module Arm
where

import Assembler
import qualified ExecutionUnit
import Loader
import Program

run :: String -> IO ()
run fileName
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> ExecutionUnit.run prog
         Right err
           -> putStrLn err