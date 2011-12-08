----------------------------------------------------------------------
-- FILE:              Debugger.hs
-- DESCRIPTION:       
-- DATE:              03/27/2001
-- PROJECT:           
-- LANGUAGE PLATFORM: 
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------

{-#OPTIONS -XFlexibleContexts #-}

module Arm.Debugger
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.IORef
import Data.Array
import Data.Array.IO
import Data.List
import ParseLib
import Data.Word
import Control.Monad.State


----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import CPU
import Decoder
import ExecutionUnit
import Format
import Loader
import Parser
import Program
import RegisterName



----------------------------------------------------------------------
-- Debugger state data structure.
----------------------------------------------------------------------
data DebugState
  = Debug
      { bkpts :: [Address]
      , radix :: Radix
      }
  deriving (Show)


dbg :: Program -> IO ()
dbg program = 
            do dbgProgram program
               return ()


----------------------------------------------------------------------
-- Debug a program, displaying the instruction at each step.
----------------------------------------------------------------------
dbgProgram :: Program -> IO CPU
dbgProgram program
  = {-let loop cpu dbgs
          = do isRunning <- readIORef (running cpu)
               if not isRunning
                 then return ()
                 else do putStr "dbg: "
                         cmd <- getLine 
                         putStrLn ""
                         case head cmd of
                           'm' -> do showMem (radix dbgs) cpu
                                     loop cpu dbgs
                           'r' -> do showRegs (radix dbgs) cpu
                                     loop cpu dbgs
                           'q' -> return ()
                           'n' -> do singleStep cpu
                                     showSurroundingInstructions (radix dbgs) cpu
                                     loop cpu dbgs
                           '?' -> do showHelp
                                     loop cpu dbgs
                           'b' -> do dbgs <- addBreakpoint dbgs
                                     loop cpu dbgs
                           's' -> do showDebugState dbgs
                                     loop cpu dbgs
                           'g' -> do runToBreakpoint cpu dbgs
                                     loop cpu dbgs
                           'h' -> do putStrLn "hex"
                                     loop cpu dbgs { radix = Hex }
                           'd' -> do putStrLn "decimal"
                                     loop cpu dbgs { radix = Dec }
                           x   -> if and [x >= '1', x <= '9']
                                    then do stepTimes cpu ((fromEnum x) - (fromEnum '0'))
                                            showSurroundingInstructions (radix dbgs) cpu
                                            loop cpu dbgs
                                    else do showSurroundingInstructions (radix dbgs) cpu
                                            loop cpu dbgs
        memSize = (memorySize program `div` 4) + 1
    in-} 
       do cpu <- (execStateT (loadProgram program) 
                             (CPU emptyMem emptyRegs True emptyCounters emptyAux)) 
          cpu' <- execStateT startRunning cpu
          --return ()
          execStateT (showSurroundingInstructions Hex) cpu'
          --loop cpu (Debug [] Hex)



----------------------------------------------------------------------
-- Run the cpu to a breakpoint, or until finished.
----------------------------------------------------------------------
{-
runToBreakpoint cpu dbgs
  = let rad = radix dbgs
        bps = bkpts dbgs
        regs = registers cpu
        loop
          = do isRunning <- readIORef (running cpu)
               if (not isRunning)
                 then return ()
                 else do pc <- getReg regs R15
                         case (elemIndex pc bps) of
                           Nothing
                             -> do singleStep cpu
                                   loop 
                           Just _
                             -> do showSurroundingInstructions rad cpu
                                   return ()
    in loop



----------------------------------------------------------------------
-- 
----------------------------------------------------------------------
stepTimes cpu n
  = if n == 0
      then return ()
      else do isRunning <- readIORef (running cpu)
              if not isRunning
                then return ()
                else do singleStep cpu
                        stepTimes cpu (n-1)



----------------------------------------------------------------------
-- Add a breakpoint to the breakpoint list.
----------------------------------------------------------------------
addBreakpoint
  :: DebugState
  -> IO DebugState

addBreakpoint dbgs
  = do putStr "break address: "
       addrStr <- getLine
       case papply pIntegral addrStr
         of [(addr, _)]
              -> return dbgs { bkpts = addr : (bkpts dbgs) }
            _ -> return dbgs



----------------------------------------------------------------------
-- Show the current debug state.
----------------------------------------------------------------------
showDebugState dbgs
  = putStrLn (show dbgs)


-}
----------------------------------------------------------------------
-- Show help message.
----------------------------------------------------------------------
showHelp :: (MonadState CPU m, MonadIO m) => m ()
showHelp
  = do liftIO $ putStrLn "  b: add breakpoint"
       liftIO $ putStrLn "  d: decimal"
       liftIO $ putStrLn "  g: go (run to next breakpoint)"
       liftIO $ putStrLn "  h: hexadecimal"
       liftIO $ putStrLn "  m: dump memory"
       liftIO $ putStrLn "  q: quit"
       liftIO $ putStrLn "  r: show registers"
       liftIO $ putStrLn "  s: show debug state"
       liftIO $ putStrLn "  1-9: step program 1-9 times"
       liftIO $ putStrLn "  ?: this help message"



----------------------------------------------------------------------
-- Show memory.
----------------------------------------------------------------------
showMem :: (MonadState CPU m, MonadIO m) => Radix -> m ()

showMem radix
  = do (lo, hi) <- getBoundM DataS -- :: IO (Int, Int)
       forM [lo .. hi] $ \addr -> do val <- readMem addr
                                     liftIO $ putStrLn (" " ++ (formatNum radix addr) ++ ": " ++ (formatNum radix val))
       return ()             


       {-let loop addr
             = do val <- readMem addr
                  if addr >= hiByte
                    then return ()
                    else do liftIO $ putStrLn (" " ++ (formatNum radix addr) ++ ": " ++ (formatNum radix val))
                            liftIO $ loop (addr + 4)
       loop lo-}



----------------------------------------------------------------------
-- Show all registers.
----------------------------------------------------------------------
showRegs :: (MonadState CPU m, MonadIO m) => Radix -> m ()
showRegs radix
  = let showReg regName
          = do regVal <- getReg regName
               liftIO $ putStr ((show regName) ++ "=" ++ (formatNum radix regVal))
    in do { liftIO $ putStr "  "; showReg R0; liftIO $ putStr "  "; showReg R4; 
            liftIO $ putStr "   "; showReg R8; liftIO $ putStr "  "; showReg R12; liftIO $ putStrLn "";
            liftIO $ putStr "  "; showReg R1; liftIO $ putStr "  "; showReg R5; 
            liftIO $ putStr "   "; showReg R9; liftIO $ putStr "  "; showReg R13; liftIO $ putStrLn "";
            liftIO $ putStr "  "; showReg R2; liftIO $ putStr "  "; showReg R6; 
            liftIO $ putStr "  "; showReg R10; liftIO $ putStr "  "; showReg R14; liftIO $ putStrLn "";
            liftIO $ putStr "  "; showReg R3; liftIO $ putStr "  "; showReg R7; 
            liftIO $ putStr "  "; showReg R11; liftIO $ putStr "  "; showReg R15; liftIO $ putStrLn "";
            showReg CPSR; liftIO $ putStr " ("; showCPSRFlags; liftIO $ putStrLn ")" }


----------------------------------------------------------------------
-- Show instructions before and after current instruction.
----------------------------------------------------------------------
showSurroundingInstructions :: (MonadState CPU m, MonadIO m) => Radix -> m ()
showSurroundingInstructions radix
  = do r15 <- getReg R15
       let pc      = fromIntegral r15
       bounds <- getBoundM CodeS
       let hiBound = fromIntegral (snd bounds) 
       let addrsLo = dropWhile (< 0) [pc - 20, pc - 16 .. pc - 4]
       let shLo    = map (showInstruction radix False) (map fromIntegral addrsLo)
       let addrsHi = takeWhile (< hiBound) [pc + 4, pc + 8 .. pc + 20]
       let shHi    = map (showInstruction radix False) (map fromIntegral addrsHi)
       sequence shLo
       showInstruction radix True (fromIntegral pc)
       sequence shHi
       return ()
    


----------------------------------------------------------------------
-- Show current instruction (highlighted).
----------------------------------------------------------------------
showInstruction :: (MonadState CPU m, MonadIO m) => Radix -> Bool -> Address -> m ()
showInstruction radix highlight addr
  = do opcode <- readMem addr
       let instr = decode opcode
       let hexOp = formatHex 8 '0' "" opcode
       liftIO $ putStr ((if highlight then ">" else " ") ++ (formatNum radix addr) ++ ": "
                 ++ (formatNum radix opcode) ++ " " ++ (if highlight then ">" else " "))
       case instr of
         Nothing
           -> liftIO $ putStrLn ""
         Just instr'
           -> liftIO $ putStrLn (show instr')



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------