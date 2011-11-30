----------------------------------------------------------------------
-- FILE:              Loader.hs
-- DATE:              03/07/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Loader
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Word
import Data.Char



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.CPU
import Arm.Encoder
import Arm.Format
import Arm.Instruction
import Arm.Memory
import Arm.Program
import Arm.Register
import Arm.RegisterName



----------------------------------------------------------------------
-- Load a program into a CPU.
----------------------------------------------------------------------
loadProgram
  :: CPU
  -> Program
  -> IO ()

loadProgram cpu program
  = let mem    = memory cpu
        regs   = registers cpu
        org    = origin program
        instrs = instructions program
        consts = constants program
    in do loadRegisters regs (regInit program)
          setReg regs R15 org
          loadInstructions mem org instrs
          loadConstants mem consts



----------------------------------------------------------------------
-- Load register pre-load values.
----------------------------------------------------------------------
loadRegisters
  :: Registers
  -> [(RegisterName, Word32)]
  -> IO ()

loadRegisters regs []
  = return ()

loadRegisters regs ((regName, val) : rest)
  = do setReg regs regName val
       loadRegisters regs rest



----------------------------------------------------------------------
-- Load a list of instructions into memory.
----------------------------------------------------------------------
loadInstructions
  :: Memory
  -> Address
  -> [Instruction]
  -> IO ()

loadInstructions mem _ []
  = return ()

loadInstructions mem addr (ins : inss)
  = do let opcode = encode ins
       writeMem mem addr opcode
       loadInstructions mem (addr + 4) inss



----------------------------------------------------------------------
-- Load a list of constant tuples into memory.
----------------------------------------------------------------------
loadConstants
  :: Memory
  -> [(Address, Constant)]
  -> IO ()

loadConstants mem []
  = return ()

loadConstants mem ((addr, const) : consts)
  = do loadConstant mem addr const
       loadConstants mem consts



----------------------------------------------------------------------
-- Load an arbitrary constant into memory.
----------------------------------------------------------------------
loadConstant
  :: Memory
  -> Address
  -> Constant
  -> IO ()

loadConstant mem addr (Array count value)
  = loadArray mem addr count value

loadConstant mem addr (Int i)
  = writeMem mem addr (fromIntegral i)

loadConstant mem addr (List l)
  = loadList mem addr l

loadConstant mem addr (String s)
  = loadString mem addr (s ++ [chr 0])

loadConstant mem addr (Word w)
  = writeMem mem addr w



----------------------------------------------------------------------
-- Load an array of constants into memory.
----------------------------------------------------------------------
loadArray
  :: Memory
  -> Address
  -> Word32
  -> Constant
  -> IO ()

loadArray mem addr 0 const
  = return ()

loadArray mem addr count const
  = do loadConstant mem addr const
       loadArray mem (addr + constSize const) (count - 1) const



----------------------------------------------------------------------
-- Load a list of constants into memory.
----------------------------------------------------------------------
loadList
  :: Memory
  -> Address
  -> [Constant]
  -> IO ()

loadList mem addr []
  = return ()

loadList mem addr (const : consts)
  = do loadConstant mem addr const
       let addr' = constSize const + addr
       loadList mem addr' consts



----------------------------------------------------------------------
-- Load a string into memory; null terminate the string.
----------------------------------------------------------------------
loadString
  :: Memory
  -> Address
  -> String
  -> IO ()

loadString mem addr []
  = return ()

loadString mem addr [c1]
  = let w = fromIntegral (ord c1)
    in writeMem mem addr w

loadString mem addr [c1, c2]
  = let w = (fromIntegral (ord c2) `shiftL` 8)
            .|. (fromIntegral (ord c1))
    in writeMem mem addr w

loadString mem addr [c1, c2, c3]
  = let w = (fromIntegral (ord c3) `shiftL` 16)
            .|. (fromIntegral (ord c2) `shift` 8)
            .|. (fromIntegral (ord c1))
    in writeMem mem addr w

loadString mem addr (c1 : c2 : c3 : c4 : cs)
  = let w = (fromIntegral (ord c4) `shiftL` 24)
            .|. (fromIntegral (ord c3) `shiftL` 16)
            .|. (fromIntegral (ord c2) `shiftL` 8)
            .|. (fromIntegral (ord c1))
    in do writeMem mem addr w
          loadString mem (addr + 4) cs



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
