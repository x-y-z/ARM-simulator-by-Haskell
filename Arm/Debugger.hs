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
-- modified by Zi Yan 12/2011
-- uses monadstate and monadio instead
----------------------------------------------------------------------
{-#OPTIONS -XFlexibleContexts #-}

module Debugger (debug)
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
import Memory (Address, emptyMem, Segment (DataS, CodeS))
import Register (emptyRegs)
import Decoder
import ExecutionUnit
import Format
import Loader
import Parser
import Program
import RegisterName
import Arm hiding (singleStep)
import Stage

-- | auxilary loop function
--   keep executing "m a" until "m Bool" gives False 
whileM :: Monad m => m Bool -> m a -> m [a]
whileM  p f = do
        x <- p
        if x
        then do
                x  <- f
                xs <- whileM p f
                return (return x `mplus` xs)
        else return mzero


singleStep :: (MonadState CPU m, MonadIO m) => m ()
singleStep
  = do pc <- getReg R15
       opcode <- readMem pc
       let instr = Decoder.decode opcode
       case instr of
         Nothing
           -> do fail ("ERROR: can't decode instruction " ++ 
                      (formatHex 8 '0' "" opcode)
                           ++ " at adddress " ++ show pc ++ " (dec)")
         Just instr'
           -> do setReg R15 (pc + 4)
                 eval instr'

----------------------------------------------------------------------
-- Debugger state data structure.
----------------------------------------------------------------------
data DebugState
  = Debug
      { bkpts :: [Address]
      , radix :: Radix
      , cmds  :: Char
      }
  deriving (Show)


debug :: Program -> IO ()
debug program = 
            do dbgProgram program
               return ()


----------------------------------------------------------------------
-- Debug a program, displaying the instruction at each step.
----------------------------------------------------------------------
dbgProgram :: Program -> IO CPU
dbgProgram program
  = do cpu <- (execStateT (loadProgram program) 
                          (CPU (emptyMem []) 
                               emptyRegs 
                               (D True) 
                               emptyCounters_ 
                               emptyAux)) 
       cpu' <- execStateT startRunning cpu
       execStateT (dbgStep (Debug [] Hex '?')) cpu'

dbgStep :: (MonadState CPU m, MonadIO m) => DebugState -> m ()
dbgStep dbgs = do r <- isRunning 
                  if r 
                  then do liftIO $ putStr "dbg: "
                          cmd <- liftIO $ getLine 
                          liftIO $ putStrLn ""
                          let cmd' = checkCmd cmd
                          case head cmd' of
                           'm' -> do showMem (radix dbgs) 
                                     dbgStep dbgs {cmds = head cmd'}
                           'r' -> do showRegs (radix dbgs) 
                                     dbgStep dbgs {cmds = head cmd'}
                           'q' -> return ()
                           'n' -> do singleStep
                                     showSurroundingInstructions (radix dbgs)
                                     dbgStep dbgs {cmds = head cmd'}
                           '?' -> do showHelp
                                     dbgStep dbgs {cmds = head cmd'}
                           'b' -> do dbgs' <- liftIO $ addBreakpoint dbgs
                                     dbgStep dbgs' {cmds = head cmd'}
                           's' -> do liftIO $ showDebugState dbgs
                                     dbgStep dbgs {cmds = head cmd'}
                           'g' -> do runToBreakpoint dbgs
                                     dbgStep dbgs {cmds = head cmd'}
                           'h' -> do liftIO $ putStrLn "hex"
                                     dbgStep dbgs { radix = Hex, 
                                                    cmds = head cmd' }
                           'd' -> do liftIO $ putStrLn "decimal"
                                     dbgStep dbgs { radix = Dec, 
                                                    cmds = head cmd' }
                           x   -> if and [x >= '1', x <= '9']
                                    then do stepTimes ((fromEnum x) - 
                                                       (fromEnum '0'))
                                            showSurroundingInstructions 
                                                (radix dbgs)
                                            dbgStep dbgs {cmds = head cmd'}
                                    else do showSurroundingInstructions 
                                                (radix dbgs)
                                            dbgStep dbgs {cmds = head cmd'}
                  else return ()
    where checkCmd :: String -> String
          checkCmd c | null c = (cmds dbgs):c
                     | otherwise = c


----------------------------------------------------------------------
-- Run the cpu to a breakpoint, or until finished.
----------------------------------------------------------------------
runToBreakpoint :: (MonadState CPU m, MonadIO m) => DebugState -> m ()
runToBreakpoint dbgs
  = let rad = radix dbgs
        bps = bkpts dbgs
    in do whileM isRunning (do pc <- getReg R15
                               case (elemIndex pc bps) of
                                    Nothing -> do singleStep
                                    Just _  -> do showSurroundingInstructions 
                                                      rad
                                                  return ())
          return ()



----------------------------------------------------------------------
-- 
----------------------------------------------------------------------
stepTimes n
  = if n == 0
      then return ()
      else do isR <- isRunning
              if isR
                then do singleStep
                        stepTimes (n-1)
                else return ()



----------------------------------------------------------------------
-- Add a breakpoint to the breakpoint list.
----------------------------------------------------------------------
addBreakpoint :: DebugState -> IO DebugState
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
showDebugState :: DebugState -> IO ()
showDebugState dbgs = putStrLn (show dbgs)



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
       forM [lo .. hi] 
            $ \addr -> do val <- readMem addr
                          liftIO $ putStrLn (" " ++ 
                                             (formatNum radix addr) ++ 
                                             ": " ++ 
                                             (formatNum radix val))
       return ()             


----------------------------------------------------------------------
-- Show all registers.
----------------------------------------------------------------------
showRegs :: (MonadState CPU m, MonadIO m) => Radix -> m ()
showRegs radix
  = let showReg regName
          = do regVal <- getReg regName
               liftIO $ putStr ((show regName) ++ "=" ++ 
                                (formatNum radix regVal))
    in do { liftIO $ putStr "  "; showReg R0; 
            liftIO $ putStr "  "; showReg R4; 
            liftIO $ putStr "   "; showReg R8; 
            liftIO $ putStr "  "; showReg R12; 
            liftIO $ putStrLn "";
            liftIO $ putStr "  "; showReg R1; 
            liftIO $ putStr "  "; showReg R5; 
            liftIO $ putStr "   "; showReg R9; 
            liftIO $ putStr "  "; showReg R13; 
            liftIO $ putStrLn "";
            liftIO $ putStr "  "; showReg R2; 
            liftIO $ putStr "  "; showReg R6; 
            liftIO $ putStr "  "; showReg R10; 
            liftIO $ putStr "  "; showReg R14; 
            liftIO $ putStrLn "";
            liftIO $ putStr "  "; showReg R3; 
            liftIO $ putStr "  "; showReg R7; 
            liftIO $ putStr "  "; showReg R11; 
            liftIO $ putStr "  "; showReg R15; 
            liftIO $ putStrLn "";
            showReg CPSR; 
            liftIO $ putStr " ("; showCPSRFlags; liftIO $ putStrLn ")" }


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
       let shLo    = map (showInstruction radix False) 
                         (map fromIntegral addrsLo)
       let addrsHi = takeWhile (< hiBound) [pc + 4, pc + 8 .. pc + 20]
       let shHi    = map (showInstruction radix False) 
                         (map fromIntegral addrsHi)
       sequence shLo
       showInstruction radix True (fromIntegral pc)
       sequence shHi
       return ()
    


----------------------------------------------------------------------
-- Show current instruction (highlighted).
----------------------------------------------------------------------
showInstruction :: (MonadState CPU m, MonadIO m) => 
                   Radix -> Bool -> Address -> m ()
showInstruction radix highlight addr
  = do opcode <- readMem addr
       let instr = Decoder.decode opcode
       let hexOp = formatHex 8 '0' "" opcode
       liftIO $ putStr ((if highlight then ">" else " ") ++ 
                        (formatNum radix addr) ++ ": " ++ 
                        (formatNum radix opcode) ++ 
                        " " ++ 
                        (if highlight then ">" else " "))
       case instr of
         Nothing
           -> liftIO $ putStrLn ""
         Just instr'
           -> liftIO $ putStrLn (show instr')



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------