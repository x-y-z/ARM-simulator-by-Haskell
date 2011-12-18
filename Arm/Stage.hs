{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-#OPTIONS -XRankNTypes -XFlexibleContexts -XImpredicativeTypes#-}
module Stage
where       
  
import Data.Word  
  
import Control.Monad.State  
  
import CPU  
import qualified Decoder
import ExecutionUnit
import Instruction
import RegisterName

type Pipeline = [Stage]

-- Any function using the CPU monad can be inserted into a pipeline
type Stage = (MonadState CPU m, MonadIO m) => m ()

-- Simple, in-order pipeline with 5 stages
inOrder :: Pipeline
inOrder = [memWrite,memRead,execute,decode,fetch]

-- Fetches an instruction from the current PC (unless stalled)
fetch :: Stage
fetch = do pc <- getReg R15
           nextCycle
           cyc <- currentCycle
           sta <- stallCycle
           df  <- getCounter "DecodeFailures"
           -- Different from other stall checks because we update
           -- the cycle counter before checking, unlike other stages
           if cyc > sta then do opcode <- readMem pc
                                if opcode == 0 then 
                                  do setReg R15 (pc + 4)
                                     incrCounter "DecodeFailures"
                                     if df > 10 then
                                       do stopRunning
                                          return ()
                                       else return ()
                                  else do setFD opcode
                                          setReg R15 (pc + 4) 
             else return ()

-- Decodes an instruction (unless stalled)
decode :: Stage
decode = do cyc <- currentCycle
            sta <- stallCycle
            if cyc >= sta then
              do opcode <- getFD
                 case opcode of
                   Nothing -> return ()
                   Just op -> case (Decoder.decode op) of
                     Nothing -> fail "Unable to decode"
                     Just i -> do setDE i 
              else return ()

-- Executes an instruction (unless stalled)
execute :: Stage
execute = do cyc <- currentCycle
             sta <- stallCycle
             if cyc >= sta then
               do instr <- getDE
                  case instr of
                    Nothing -> return ()
                    Just i -> do evalInO i
                                 executedInstr
               else return ()

-- Processes any loads that have been added from the execute stage
memRead :: Stage
memRead = do ld <- getLoad
             case ld of
               Nothing -> return ()
               Just (r,a) -> do l <- loadCache a
                                if l > 1 then
                                  do stallForN l
                                     v <- readMem a
                                     setReg r v
                                  else
                                  do v <- readMem a
                                     setReg r v

-- Processes any stores that have been added from the execute stage
memWrite :: Stage
memWrite = do st <- getStore
              case st of
                Nothing -> return ()
                Just (v,a) -> do writeMem a v

-- Insert a fetched instruction into the latch between fetch and decode
setFD :: (MonadState CPU m, MonadIO m) => Word32 -> m ()
setFD i = do (CPU _ _ _ _ a) <- get
             case a of
                  Nil -> fail "Need In-Order auxilary data"
                  (InO f d m w) -> setAuxilary (InO (f ++ [i]) d m w)
                  
-- Retrives a fetched, un-decoded instruction
getFD :: (MonadState CPU m, MonadIO m) => m (Maybe Word32)
getFD = do (CPU _ _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO f d m w) -> case f of
               [] -> return Nothing
               (x : xs) -> do setAuxilary (InO xs d m w)
                              return (Just x)

-- Insert a decoded instruction into the latch between decode and execute
setDE :: (MonadState CPU m, MonadIO m) => Instruction -> m ()
setDE i = do (CPU _ _ _ _ a) <- get
             case a of
               Nil -> fail "Need In-Order auxilary data"
               (InO f d m w) -> setAuxilary (InO f (d ++ [i]) m w)

-- Retrieve a decoded instruction
getDE :: (MonadState CPU m, MonadIO m) => m (Maybe Instruction)
getDE = do (CPU _ _ _ _ a) <- get
           case a of
             Nil -> fail "Need In-Order auxilary data"
             (InO f d m w) -> case d of
               [] -> return Nothing
               (x : xs) -> do setAuxilary (InO f xs m w)
                              return (Just x)

-- Model of execution that completes in a single Stage
-- Most closely models a multiple cycle execution
simplePipe :: Pipeline
simplePipe = [singleStage]

-- Executes pipeline in a single step
singleStage :: Stage
singleStage = do pc <- getReg R15
                 nextCycle
                 df <- getCounter "DecodeFailures"
                 opcode <- readMem pc
                 if opcode == 0 then do setReg R15 (pc + 4)
                                        incrCounter "DecodeFailures"
                                        if df > 10 then
                                          do stopRunning
                                             return () 
                                          else return ()
                                        else
                   do nextCycle
                      case (Decoder.decode opcode) of
                        Nothing -> incrCounter "DecodeFailures"
                        Just i -> do setReg R15 (pc + 4)
                                     nextCycle
                                     eval i
                                     executedInstr