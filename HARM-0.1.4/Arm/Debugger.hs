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



module Arm.Debugger
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.IORef
import Data.Array
import Data.Array.IO
import Data.List
import Arm.ParseLib
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.CPU
import Arm.Decoder
import Arm.ExecutionUnit
import Arm.Format
import Arm.Loader
import Arm.Memory
import Arm.Parser
import Arm.Program
import Arm.Register
import Arm.RegisterName



----------------------------------------------------------------------
-- Debugger state data structure.
----------------------------------------------------------------------
data DebugState
  = Debug
      { bkpts :: [Address]
      , radix :: Radix
      }
  deriving (Show)



----------------------------------------------------------------------
-- Debug a program, displaying the instruction at each step.
----------------------------------------------------------------------
dbg
  :: Program
  -> IO ()

dbg program
  = let loop cpu dbgs
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
    in do cpu <- emptyCPU memSize
          writeIORef (debug cpu) True
          loadProgram cpu program
          showSurroundingInstructions Hex cpu
          loop cpu (Debug [] Hex)



----------------------------------------------------------------------
-- Run the cpu to a breakpoint, or until finished.
----------------------------------------------------------------------
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



----------------------------------------------------------------------
-- Show help message.
----------------------------------------------------------------------
showHelp
  :: IO ()

showHelp
  = do putStrLn "  b: add breakpoint"
       putStrLn "  d: decimal"
       putStrLn "  g: go (run to next breakpoint)"
       putStrLn "  h: hexadecimal"
       putStrLn "  m: dump memory"
       putStrLn "  q: quit"
       putStrLn "  r: show registers"
       putStrLn "  s: show debug state"
       putStrLn "  1-9: step program 1-9 times"
       putStrLn "  ?: this help message"



----------------------------------------------------------------------
-- Show memory.
----------------------------------------------------------------------
showMem
  :: Radix
  -> CPU
  -> IO ()

showMem radix cpu
  = do let mem = memory cpu
       (lo, hi) <- getBounds mem -- :: IO (Int, Int)
       let hiByte = hi * 4
       let loop addr
             = do val <- readMem mem addr
                  if addr >= hiByte
                    then return ()
                    else do putStrLn (" " ++ (formatNum radix addr) ++ ": " ++ (formatNum radix val))
                            loop (addr + 4)
       loop lo



----------------------------------------------------------------------
-- Show all registers.
----------------------------------------------------------------------
showRegs
  :: Radix
  -> CPU
  -> IO ()

showRegs radix cpu
  = let regs = registers cpu
        showReg regName
          = do regVal <- getReg regs regName
               putStr ((show regName) ++ "=" ++ (formatNum radix regVal))
    in do { putStr "  "; showReg R0; putStr "  "; showReg R4; putStr "   "; showReg R8; putStr "  "; showReg R12; putStrLn "";
            putStr "  "; showReg R1; putStr "  "; showReg R5; putStr "   "; showReg R9; putStr "  "; showReg R13; putStrLn "";
            putStr "  "; showReg R2; putStr "  "; showReg R6; putStr "  "; showReg R10; putStr "  "; showReg R14; putStrLn "";
            putStr "  "; showReg R3; putStr "  "; showReg R7; putStr "  "; showReg R11; putStr "  "; showReg R15; putStrLn "";
            showReg CPSR; putStr " ("; showCPSRFlags regs; putStrLn ")" }



----------------------------------------------------------------------
-- Show instructions before and after current instruction.
----------------------------------------------------------------------
showSurroundingInstructions radix cpu
  = do let regs = registers cpu
       r15 <- getReg regs R15
       let pc      = fromIntegral r15
       let mem     = memory cpu
       -- let bounds  = range mem
       bounds <- getBounds mem
       let hiBound = fromIntegral (snd bounds) * 4
       let addrsLo = dropWhile (< 0) [pc - 20, pc - 16 .. pc - 4]
       let shLo    = map (showInstruction radix mem False) (map fromIntegral addrsLo)
       let addrsHi = takeWhile (< hiBound) [pc + 4, pc + 8 .. pc + 20]
       let shHi    = map (showInstruction radix mem False) (map fromIntegral addrsHi)
       sequence shLo
       showInstruction radix mem True (fromIntegral pc)
       sequence shHi
        


----------------------------------------------------------------------
-- Show current instruction (highlighted).
----------------------------------------------------------------------
showInstruction
  :: Radix
  -> Memory
  -> Bool
  -> Address
  -> IO ()

showInstruction radix mem highlight addr
  = do opcode <- readMem mem addr
       let instr = decode opcode
       let hexOp = formatHex 8 '0' "" opcode
       putStr ((if highlight then ">" else " ") ++ (formatNum radix addr) ++ ": "
                 ++ (formatNum radix opcode) ++ " " ++ (if highlight then ">" else " "))
       case instr of
         Nothing
           -> putStrLn ""
         Just instr'
           -> putStrLn (show instr')



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
