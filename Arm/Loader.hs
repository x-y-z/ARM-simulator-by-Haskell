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
loadProgram :: Program -> State CPU ()
loadProgram program = let org    = origin program
                          instrs = instructions program
                          consts = constants program
    in do (CPU mem regs) <- get
          loadRegisters (regInit program)
          setReg R15 org
          loadInstructions org instrs
          loadConstants consts
          
----------------------------------------------------------------------
-- Load register pre-load values.
----------------------------------------------------------------------
loadRegisters :: [(RegisterName, Word32)] -> State CPU ()
loadRegisters [] = return ()
loadRegisters ((regName, val) : rest) = do setReg regName val
                                           loadRegisters rest

----------------------------------------------------------------------
-- Load a list of instructions into memory.
----------------------------------------------------------------------
loadInstructions :: Address -> [Instruction] -> State CPU ()
loadInstructions _ [] = return ()
loadInstructions addr (ins : inss)
  = do let opcode = encode ins
       writeMem addr opcode
       loadInstructions (addr + 4) inss



----------------------------------------------------------------------
-- Load a list of constant tuples into memory.
----------------------------------------------------------------------
loadConstants :: [(Address, Constant)] -> State CPU ()
loadConstants [] = return ()

loadConstants ((addr, const) : consts)
  = do loadConstant addr const
       loadConstants consts



----------------------------------------------------------------------
-- Load an arbitrary constant into memory.
----------------------------------------------------------------------
loadConstant :: Address -> Constant -> State CPU ()

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
loadArray :: Address -> Word32 -> Constant -> State CPU ()
loadArray addr 0 const
  = return ()

loadArray addr count const
  = do loadConstant addr const
       loadArray (addr + constSize const) (count - 1) const



----------------------------------------------------------------------
-- Load a list of constants into memory.
----------------------------------------------------------------------
loadList :: Address -> [Constant] -> State CPU ()
loadList addr []
  = return ()

loadList addr (const : consts)
  = do loadConstant addr const
       let addr' = constSize const + addr
       loadList addr' consts



----------------------------------------------------------------------
-- Load a string into memory; null terminate the string.
----------------------------------------------------------------------
loadString :: Address -> String -> State CPU ()
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