----------------------------------------------------------------------
-- FILE:              Arm.hs
-- DESCRIPTION:       Main file for running and debugging ARM
--                    assembly source files.
-- DATE:              04/04/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: Hugs
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Arm
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Assembler
import qualified Arm.Debugger
import qualified Arm.ExecutionUnit
import Arm.Loader
import Arm.Program



----------------------------------------------------------------------
-- Run a program.
----------------------------------------------------------------------
run
  :: String     -- program's file name
  -> IO ()

run fileName
  = do progOrError <- asmFile fileName
       case progOrError of
         Left prog
           -> Arm.ExecutionUnit.run prog
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
           -> Arm.Debugger.dbg prog
         Right err
           -> putStrLn err



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
