----------------------------------------------------------------------
-- FILE:              ExecutionUnit.hs
-- DATE:              2/6/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.ExecutionUnit
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Int
import Data.IORef
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Data.Bits
import Arm.CPU
import Arm.Decoder
import Arm.Format
import Arm.Instruction
import Arm.Loader
import Arm.Memory
import Arm.Operand
import Arm.Program
import Arm.Register
import Arm.RegisterName
import Arm.Swi



----------------------------------------------------------------------
-- Evaluate a single instruction.
----------------------------------------------------------------------
eval
  :: CPU
  -> Instruction
  -> IO ()

-- add two registers
eval cpu (Add (Reg reg1) (Reg reg2) (Reg reg3))
  = do let regs = registers cpu
       r2 <- getReg regs reg2
       r3 <- getReg regs reg3
       setReg regs reg1 (r2 + r3)

eval cpu (Add (Reg reg1) (Reg reg2) (Con con1))
  = do let regs = registers cpu
       r2 <- getReg regs reg2
       setReg regs reg1 (r2 + con1)

-- logical bit-wise and
eval cpu (And (Reg reg1) (Reg reg2) (Reg reg3))
  = do let regs = registers cpu
       r2 <- getReg regs reg2
       r3 <- getReg regs reg3
       setReg regs reg1 (r2 .&. r3)

-- branch unconditionally
eval cpu (B (Rel offset))
  = do let regs = registers cpu
       pc <- getReg regs R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       setReg regs R15 pc''

-- branch if equal
eval cpu (Beq (Rel offset))
  = do let regs = registers cpu
       pc <- getReg regs R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       z <- cpsrGetZ regs
       if z == 1
         then setReg regs R15 pc''
         else return ()

-- branch if greater than
eval cpu (Bgt (Rel offset))
  = do let regs = registers cpu
       pc <- getReg regs R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       c <- cpsrGetC regs
       if c == 1
         then setReg regs R15 pc''
         else return ()

-- bit clear
eval cpu (Bic (Reg reg1) (Reg reg2) (Reg reg3))
  = do let regs = registers cpu
       r2 <- getReg regs reg2
       r3 <- getReg regs reg3
       setReg regs reg1 (r2 .&. (complement r3))

-- branch and link
eval cpu (Bl (Rel offset))
  = do let regs = registers cpu
       pc <- getReg regs R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       setReg regs R14 pc
       setReg regs R15 pc''

-- branch if less than
eval cpu (Blt (Rel offset))
  = do let regs = registers cpu
       pc <- getReg regs R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       n <- cpsrGetN regs
       if n == 1
         then setReg regs R15 pc''
         else return ()

-- branch if not equal
eval cpu (Bne (Rel offset))
  = do let regs = registers cpu
       pc <- getReg regs R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       z <- cpsrGetZ regs
       if z == 0
         then setReg regs R15 pc''
         else return ()

-- compare two values
eval cpu (Cmp (Reg reg1) op2)
  = do let regs = registers cpu
       r1 <- getReg regs reg1
       let val1 = fromIntegral r1
       val2 <- case op2 of
                 Con c -> return (fromIntegral c)
                 Reg r -> do r' <- getReg regs r
                             return (fromIntegral r')
       setReg regs CPSR 0
       if val1 < val2
         then cpsrSetN regs
         else if val1 == val2
                then cpsrSetZ regs
                else cpsrSetC regs

-- logical bit-wise exclusive or
eval cpu (Eor (Reg reg1) (Reg reg2) (Reg reg3))
  = do let regs = registers cpu
       r2 <- getReg regs reg2
       r3 <- getReg regs reg3
       setReg regs reg1 (r2 `xor` r3)

-- load multiple registers, empty ascending
eval cpu (Ldmea op1 (Mrg regList))
  = do let regs = registers cpu
       let mem = memory cpu
       let (reg, writeBack) = case op1 of { Aut (Reg r) -> (r, True); Reg r -> (r, False) }
       addr <- getReg regs reg
       let loadRegs addr []
             = return (addr + 4)
           loadRegs addr (r : rs)
             = do val <- readMem mem addr
                  setReg regs r val
                  loadRegs (addr - 4) rs
       addr' <- loadRegs (addr - 4) (reverse regList)
       if writeBack
         then setReg regs reg addr'
         else return ()
{-
-- load register, indirect
eval cpu (Ldr (Reg reg1) (Ind reg2))
  = do let regs = registers cpu
       let mem  = memory cpu
       addr <- getReg regs reg2
       val  <- readMem mem addr
       setReg regs reg1 val

-- load register, base + offset
eval cpu (Ldr (Reg reg1) (Bas reg2 offset))
  = do let regs = registers cpu
       let mem  = memory cpu
       addr <- getReg regs reg2
       val  <- readMem mem (addr + offset)
       setReg regs reg1 val

-- load register, auto-indexed
eval cpu (Ldr (Reg reg1) (Aut (Bas reg2 offset)))
  = do let regs  = registers cpu
       let mem   = memory cpu
       addr <- getReg regs reg2
       val  <- readMem mem (addr + offset)
       setReg regs reg2 (addr + offset)  -- write the address back into reg2
       setReg regs reg1 val

-- load register, post-indexed
eval cpu (Ldr (Reg reg1) (Pos (Ind reg2) offset))
  = do let regs  = registers cpu
       let mem   = memory cpu
       addr <- getReg regs reg2
       val  <- readMem mem addr
       setReg regs reg2 (addr + offset)  -- write addr + offset back into reg2
       setReg regs reg1 val
-}
-- load register
eval cpu (Ldr (Reg reg1) op2)
  = do let regs  = registers cpu
       let mem   = memory cpu
       val <- case op2 of
                Ind reg2
                  -> do addr <- getReg regs reg2
                        readMem mem addr
                Bas reg2 offset
                  -> do addr <- getReg regs reg2
                        readMem mem (addr + offset)
                Aut (Bas reg2 offset)
                  -> do addr <- getReg regs reg2
                        setReg regs reg2 (addr + offset)  -- write the address back into reg2
                        readMem mem (addr + offset)
                Pos (Ind reg2) offset
                  -> do addr <- getReg regs reg2
                        setReg regs reg2 (addr + offset)  -- write addr + offset back into reg2
                        readMem mem addr
       setReg regs reg1 val

-- load register, unsigned byte
eval cpu (Ldrb (Reg reg1) op2)
  = do let regs  = registers cpu
       let mem   = memory cpu
       addr
         <- case op2 of
              Ind reg2
                -> do addr <- getReg regs reg2
                      return addr
              Bas reg2 offset
                -> do addr <- getReg regs reg2
                      return (addr + offset)
              Aut (Bas reg2 offset)
                -> do addr <- getReg regs reg2
                      setReg regs reg2 (addr + offset)  -- write the address back into reg2
                      return (addr + offset)
              Pos (Ind reg2) offset
                -> do addr <- getReg regs reg2
                      setReg regs reg2 (addr + offset)  -- write addr + offset back into reg2
                      return addr
       val <- readMem mem addr
       let byteOffset = fromIntegral (addr .&. 3)
       let byte = 0xFF .&. (val `shiftR` (byteOffset * 8))
       setReg regs reg1 byte

-- move constant into register
eval cpu (Mov (Reg reg) (Con con))
  = setReg (registers cpu) reg con

-- move register into register
eval cpu (Mov (Reg reg1) (Reg reg2))
  = do let regs = registers cpu
       val <- getReg regs reg2
       setReg regs reg1 val

eval cpu (Mul (Reg reg1) (Reg reg2) (Reg reg3))
  = do let regs = registers cpu
       r2 <- getReg regs reg2
       r3 <- getReg regs reg3
       let prod = (r2 * r3) .&. 0x7FFFFFFF
       setReg regs reg1 prod

-- logical bit-wise or
eval cpu (Orr (Reg reg1) (Reg reg2) (Reg reg3))
  = do let regs = registers cpu
       r2 <- getReg regs reg2
       r3 <- getReg regs reg3
       setReg regs reg1 (r2 .|. r3)

-- load multiple registers, empty ascending
eval cpu (Stmea op1 (Mrg regList))
  = do let regs = registers cpu
       let mem = memory cpu
       let (reg, writeBack) = case op1 of { Aut (Reg r) -> (r, True); Reg r -> (r, False) }
       addr <- getReg regs reg
       let storeRegs addr []
             = return addr
           storeRegs addr (r : rs)
             = do val <- getReg regs r
                  writeMem mem addr val
                  storeRegs (addr + 4) rs
       addr' <- storeRegs addr regList
       if writeBack
         then setReg regs reg addr'
         else return ()
{-
-- store register, indirect
eval cpu (Str (Reg reg1) (Ind reg2))
  = do let regs = registers cpu
       let mem  = memory cpu
       val  <- getReg regs reg1
       addr <- getReg regs reg2
       writeMem mem addr val

-- store register, base + offset
eval cpu (Str (Reg reg1) (Bas reg2 offset))
  = do let regs = registers cpu
       let mem  = memory cpu
       val  <- getReg regs reg1
       addr <- getReg regs reg2
       writeMem mem (addr + offset) val

-- store register, auto-indexed
eval cpu (Str (Reg reg1) (Aut (Bas reg2 offset)))
  = do let regs  = registers cpu
       let mem   = memory cpu
       addr <- getReg regs reg2
       let addr' = addr + offset
       r1 <- getReg regs reg1
       writeMem mem addr' r1
       setReg regs reg2 addr'  -- write the address back into reg2

-- store register, post-indexed
eval cpu (Str (Reg reg1) (Pos (Ind reg2) offset))
  = do let regs  = registers cpu
       let mem   = memory cpu
       addr <- getReg regs reg2
       val  <- getReg regs reg1
       writeMem mem addr val
       setReg regs reg2 (addr + offset)  -- write addr + offset back into reg2
-}
-- store register
eval cpu (Str (Reg reg1) op2)
  = do let regs = registers cpu
       let mem  = memory cpu
       val <- getReg regs reg1
       case op2 of
         Ind reg2
           -> do addr <- getReg regs reg2
                 writeMem mem addr val
         Aut (Bas reg2 offset)
           -> do addr <- getReg regs reg2
                 let addr' = addr + offset
                 writeMem mem addr' val
                 setReg regs reg2 addr'  -- write the address back into reg2
         Bas reg2 offset
           -> do addr <- getReg regs reg2
                 writeMem mem (addr + offset) val
         Pos (Ind reg2) offset
           -> do addr <- getReg regs reg2
                 writeMem mem addr val
                 setReg regs reg2 (addr + offset)  -- write addr + offset back into reg2

-- store register, unsigned byte
eval cpu (Strb (Reg reg1) op2)
  = do let regs = registers cpu
       let mem  = memory cpu
       val <- getReg regs reg1
       let val' = val .&. 0xFF
       case op2 of
         Ind reg2
           -> do addr <- getReg regs reg2
                 wrd <- readMem mem addr
                 let byteOffset = fromIntegral (addr .&. 3)
                 let val'' = val' `shiftL` (byteOffset * 8)
                 let mask = complement (0xFF `shiftL` (byteOffset * 8))
                 writeMem mem addr ((wrd .&. mask) .|. val'')
         Aut (Bas reg2 offset)
           -> do addr <- getReg regs reg2
                 let addr' = addr + offset
                 wrd <- readMem mem addr'
                 let byteOffset = fromIntegral (addr' .&. 3)
                 let val'' = val' `shiftL` (byteOffset * 8)
                 let mask = complement (0xFF `shiftL` (byteOffset * 8))
                 writeMem mem addr' ((wrd .&. mask) .|. val'')
                 setReg regs reg2 addr'  -- write the address back into reg2
         Bas reg2 offset
           -> do addr <- getReg regs reg2
                 let addr' = addr + offset
                 wrd <- readMem mem addr'
                 let byteOffset = fromIntegral (addr' .&. 3)
                 let val'' = val' `shiftL` (byteOffset * 8)
                 let mask = complement (0xFF `shiftL` (byteOffset * 8))
                 writeMem mem addr' ((wrd .&. mask) .|. val'')
         Pos (Ind reg2) offset
           -> do addr <- getReg regs reg2
                 wrd <- readMem mem addr
                 let byteOffset = fromIntegral (addr .&. 3)
                 let val'' = val' `shiftL` (byteOffset * 8)
                 let mask = complement (0xFF `shiftL` (byteOffset * 8))
                 writeMem mem addr ((wrd .&. mask) .|. val'')
                 setReg regs reg2 (addr + offset)  -- write addr + offset back into reg2

-- subtract two registers
eval cpu (Sub (Reg reg1) (Reg reg2) (Reg reg3))
  = do let regs = registers cpu
       r2 <- getReg regs reg2
       r3 <- getReg regs reg3
       setReg regs reg1 (r2 - r3)

-- software interrupt
eval cpu (Swi (Con isn))
  = do dbg <- readIORef (debug cpu)
       swi cpu isn dbg



----------------------------------------------------------------------
-- Run a CPU until its running flag is set to False.
----------------------------------------------------------------------
run'
  :: CPU
  -> IO ()

run' cpu
  = do isRunning <- readIORef (running cpu)
       if isRunning
         then do singleStep cpu
                 run' cpu
         else return ()



----------------------------------------------------------------------
-- 
----------------------------------------------------------------------
singleStep
  :: CPU
  -> IO ()

singleStep cpu
  = do let regs = registers cpu
       let mem  = memory cpu
       pc <- getReg regs R15
       opcode <- readMem mem pc
       let instr = decode opcode
       case instr of
         Nothing
           -> do putStrLn ("ERROR: can't decode instruction " ++ (formatHex 8 '0' "" opcode)
                           ++ " at adddress " ++ show pc ++ " (dec)")
                 let runFlag = running cpu
                 writeIORef runFlag False
         Just instr'
           -> do setReg regs R15 (pc + 4)
                 eval cpu instr'



----------------------------------------------------------------------
-- Run a program.
----------------------------------------------------------------------
run
  :: Program
  -> IO ()

run program
  = do let memSize = (memorySize program `div` 4) + 1
       cpu <- emptyCPU memSize
       loadProgram cpu program
       run' cpu



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
