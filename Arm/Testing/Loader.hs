{-#OPTIONS  -XFlexibleContexts #-}

module Loader
where
  
import Data.Bits
import Data.Word
import Data.Char

import Control.Monad.State

import CPU
import Encoder
import Format
import Instruction
import Program
import RegisterName

----------------------------------------------------------------------
-- Load a program into a CPU.
----------------------------------------------------------------------
loadProgram :: (MonadState CPU m, MonadIO m) => Program -> m ()
loadProgram program = let org    = origin program
                          instrs = instructions program
                          consts = constants program
    in do (CPU mem regs _ _ _) <- get
          loadRegisters (regInit program)
          setReg R15 org
          setReg PC org
          codeEnd <- loadInstructions org instrs
          setBoundM CodeS (org, codeEnd)
          loadConstants consts
          let dataStart = if null consts then (codeEnd + 4) else fst (head consts)
          let dataEnd = if null consts then (codeEnd + 4) else fst (last consts)
          setBoundM DataS (dataStart, dataEnd)
          
----------------------------------------------------------------------
-- Load register pre-load values.
----------------------------------------------------------------------
loadRegisters :: (MonadState CPU m, MonadIO m) => [(RegisterName, Word32)] -> m ()
loadRegisters [] = return ()
loadRegisters ((regName, val) : rest) = do setReg regName val
                                           loadRegisters rest

----------------------------------------------------------------------
-- Load a list of instructions into memory.
----------------------------------------------------------------------
loadInstructions :: (MonadState CPU m, MonadIO m) => Address -> [Instruction] -> m Word32
loadInstructions addr [] = return (addr - 4)
loadInstructions addr (ins : inss)
  = do let opcode = encode ins
       writeMem addr opcode
       loadInstructions (addr + 4) inss

----------------------------------------------------------------------
-- Load a list of constant tuples into memory.
----------------------------------------------------------------------
loadConstants :: (MonadState CPU m, MonadIO m) => [(Address, Constant)] -> m ()
loadConstants [] = return ()
loadConstants ((addr, const) : consts)
  = do loadConstant addr const
       loadConstants consts



----------------------------------------------------------------------
-- Load an arbitrary constant into memory.
----------------------------------------------------------------------
loadConstant :: (MonadState CPU m, MonadIO m) => Address -> Constant -> m ()

loadConstant addr (Array count value)
  = loadArray addr count value

loadConstant addr (Int i)
  = writeMem addr (fromIntegral i)

loadConstant addr (List l)
  = loadList addr l

loadConstant addr (String s)
  = loadString addr (s ++ [chr 0])

loadConstant addr (Word w)
  = writeMem addr w



----------------------------------------------------------------------
-- Load an array of constants into memory.
----------------------------------------------------------------------
loadArray :: (MonadState CPU m, MonadIO m) => Address -> Word32 -> Constant -> m ()
loadArray addr 0 const
  = return ()

loadArray addr count const
  = do loadConstant addr const
       loadArray (addr + constSize const) (count - 1) const



----------------------------------------------------------------------
-- Load a list of constants into memory.
----------------------------------------------------------------------
loadList :: (MonadState CPU m, MonadIO m) => Address -> [Constant] -> m ()
loadList addr []
  = return ()

loadList addr (const : consts)
  = do loadConstant addr const
       let addr' = constSize const + addr
       loadList addr' consts



----------------------------------------------------------------------
-- Load a string into memory; null terminate the string.
----------------------------------------------------------------------
loadString :: (MonadState CPU m, MonadIO m) => Address -> String -> m ()
loadString addr [] = return ()
loadString addr [c1] = let w = fromIntegral (ord c1) in writeMem addr w

loadString addr [c1, c2]
  = let w = (fromIntegral (ord c2) `shiftL` 8)
            .|. (fromIntegral (ord c1))
    in writeMem addr w

loadString addr [c1, c2, c3]
  = let w = (fromIntegral (ord c3) `shiftL` 16)
            .|. (fromIntegral (ord c2) `shift` 8)
            .|. (fromIntegral (ord c1))
    in writeMem addr w

loadString addr (c1 : c2 : c3 : c4 : cs)
  = let w = (fromIntegral (ord c4) `shiftL` 24)
            .|. (fromIntegral (ord c3) `shiftL` 16)
            .|. (fromIntegral (ord c2) `shiftL` 8)
            .|. (fromIntegral (ord c1))
    in do writeMem addr w
          loadString (addr + 4) cs