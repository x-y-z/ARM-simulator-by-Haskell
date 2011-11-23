----------------------------------------------------------------------
-- FILE:              CPU.hs
-- DATE:              02/18/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.CPU
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.IORef



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Memory
import Arm.Register



----------------------------------------------------------------------
-- CPU data type.
----------------------------------------------------------------------
data CPU
  = CPU
      { memory    :: Memory
      , registers :: Registers
      , running   :: IORef Bool
      , debug     :: IORef Bool
      }



----------------------------------------------------------------------
-- Create an empty CPU given a memory size.
----------------------------------------------------------------------
emptyCPU
  :: Address
  -> IO CPU

emptyCPU memSize
  = do mem  <- emptyMem memSize
       regs <- emptyRegs
       run  <- newIORef True
       dbg  <- newIORef False
       return CPU
                { memory    = mem
                , registers = regs
                , running   = run
                , debug     = dbg
                }



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
