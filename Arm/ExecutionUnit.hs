{-#OPTIONS  -XFlexibleContexts #-}
module ExecutionUnit
where
  
import Data.Bits
import Data.Int
import Data.IORef
import Data.Word

import Control.Monad.State

import CPU
import Decoder
import Format
import Instruction
import Loader
import Operand
import Program
import RegisterName

----------------------------------------------------------------------
-- Evaluate a single instruction.
----------------------------------------------------------------------
eval :: (MonadState CPU m, MonadIO m) => Instruction -> m ()

-- add two registers
eval (Add (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 + r3)

eval (Add (Reg reg1) (Reg reg2) (Con con1))
  = do r2 <- getReg reg2
       setReg reg1 (r2 + con1)

-- logical bit-wise and
eval (And (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 .&. r3)

-- branch unconditionally
-- TODO:  Change this to use actual PC rather than R15
eval (B (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       setReg R15 pc''

-- branch if equal
eval (Beq (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       z <- cpsrGetZ
       if z == 1
         then setReg R15 pc''
         else return ()

-- branch if greater than
eval (Bgt (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       c <- cpsrGetC
       if c == 1
         then setReg R15 pc''
         else return ()

-- bit clear
eval (Bic (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 .&. (complement r3))

-- branch and link
eval (Bl (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       setReg R14 pc
       setReg R15 pc''

-- branch if less than
eval (Blt (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       n <- cpsrGetN
       if n == 1
         then setReg R15 pc''
         else return ()

-- branch if not equal
eval (Bne (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 4
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       z <- cpsrGetZ
       if z == 0
         then setReg R15 pc''
         else return ()

-- compare two values
eval (Cmp (Reg reg1) op2)
  = do r1 <- getReg reg1
       let val1 = fromIntegral r1
       val2 <- case op2 of
                 Con c -> return (fromIntegral c)
                 Reg r -> do r' <- getReg r
                             return (fromIntegral r')
       setReg CPSR 0
       if val1 < val2
         then cpsrSetN
         else if val1 == val2
                then cpsrSetZ
                else cpsrSetC

-- logical bit-wise exclusive or
eval (Eor (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 `xor` r3)

-- load multiple registers, empty ascending
eval (Ldmea op1 (Mrg regList))
  = do let (reg, writeBack) = case op1 of { Aut (Reg r) -> (r, True); Reg r -> (r, False) }
       addr <- getReg reg
       let loadRegs addr []
             = return (addr + 4)
           loadRegs addr (r : rs)
             = do val <- readMem addr
                  setReg r val
                  loadRegs (addr - 4) rs
       addr' <- loadRegs (addr - 4) (reverse regList)
       if writeBack
         then setReg reg addr'
         else return ()
              
-- load register
eval (Ldr (Reg reg1) op2)
  = do val <- case op2 of
                Ind reg2
                  -> do addr <- getReg reg2
                        readMem addr
                Bas reg2 offset
                  -> do addr <- getReg reg2
                        readMem (addr + offset)
                Aut (Bas reg2 offset)
                  -> do addr <- getReg reg2
                        setReg reg2 (addr + offset)  -- write the address back into reg2
                        readMem (addr + offset)
                Pos (Ind reg2) offset
                  -> do addr <- getReg reg2
                        setReg reg2 (addr + offset)  -- write addr + offset back into reg2
                        readMem addr
       setReg reg1 val

-- load register, unsigned byte
eval (Ldrb (Reg reg1) op2)
  = do addr
         <- case op2 of
              Ind reg2
                -> do addr <- getReg reg2
                      return addr
              Bas reg2 offset
                -> do addr <- getReg reg2
                      return (addr + offset)
              Aut (Bas reg2 offset)
                -> do addr <- getReg reg2
                      setReg reg2 (addr + offset)  -- write the address back into reg2
                      return (addr + offset)
              Pos (Ind reg2) offset
                -> do addr <- getReg reg2
                      setReg reg2 (addr + offset)  -- write addr + offset back into reg2
                      return addr
       val <- readMem addr
       let byteOffset = fromIntegral (addr .&. 3)
       let byte = 0xFF .&. (val `shiftR` (byteOffset * 8))
       setReg reg1 byte

-- move constant into register
eval (Mov (Reg reg) (Con con))
  = setReg reg con

-- move register into register
eval (Mov (Reg reg1) (Reg reg2))
  = do val <- getReg reg2
       setReg reg1 val

eval (Mul (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       let prod = (r2 * r3) .&. 0x7FFFFFFF
       setReg reg1 prod

-- logical bit-wise or
eval (Orr (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 .|. r3)

-- load multiple registers, empty ascending
eval (Stmea op1 (Mrg regList))
  = do let (reg, writeBack) = case op1 of { Aut (Reg r) -> (r, True); Reg r -> (r, False) }
       addr <- getReg reg
       let storeRegs addr []
             = return addr
           storeRegs addr (r : rs)
             = do val <- getReg r
                  writeMem addr val
                  storeRegs (addr + 4) rs
       addr' <- storeRegs addr regList
       if writeBack
         then setReg reg addr'
         else return ()

-- store register
eval (Str (Reg reg1) op2)
  = do val <- getReg reg1
       case op2 of
         Ind reg2
           -> do addr <- getReg reg2
                 writeMem addr val
         Aut (Bas reg2 offset)
           -> do addr <- getReg reg2
                 let addr' = addr + offset
                 writeMem addr' val
                 setReg reg2 addr'  -- write the address back into reg2
         Bas reg2 offset
           -> do addr <- getReg reg2
                 writeMem (addr + offset) val
         Pos (Ind reg2) offset
           -> do addr <- getReg reg2
                 writeMem addr val
                 setReg reg2 (addr + offset)  -- write addr + offset back into reg2

-- store register, unsigned byte
eval (Strb (Reg reg1) op2)
  = do val <- getReg reg1
       let val' = val .&. 0xFF
       case op2 of
         Ind reg2
           -> do addr <- getReg reg2
                 wrd <- readMem addr
                 let byteOffset = fromIntegral (addr .&. 3)
                 let val'' = val' `shiftL` (byteOffset * 8)
                 let mask = complement (0xFF `shiftL` (byteOffset * 8))
                 writeMem addr ((wrd .&. mask) .|. val'')
         Aut (Bas reg2 offset)
           -> do addr <- getReg reg2
                 let addr' = addr + offset
                 wrd <- readMem addr'
                 let byteOffset = fromIntegral (addr' .&. 3)
                 let val'' = val' `shiftL` (byteOffset * 8)
                 let mask = complement (0xFF `shiftL` (byteOffset * 8))
                 writeMem addr' ((wrd .&. mask) .|. val'')
                 setReg reg2 addr'  -- write the address back into reg2
         Bas reg2 offset
           -> do addr <- getReg reg2
                 let addr' = addr + offset
                 wrd <- readMem addr'
                 let byteOffset = fromIntegral (addr' .&. 3)
                 let val'' = val' `shiftL` (byteOffset * 8)
                 let mask = complement (0xFF `shiftL` (byteOffset * 8))
                 writeMem addr' ((wrd .&. mask) .|. val'')
         Pos (Ind reg2) offset
           -> do addr <- getReg reg2
                 wrd <- readMem addr
                 let byteOffset = fromIntegral (addr .&. 3)
                 let val'' = val' `shiftL` (byteOffset * 8)
                 let mask = complement (0xFF `shiftL` (byteOffset * 8))
                 writeMem addr ((wrd .&. mask) .|. val'')
                 setReg reg2 (addr + offset)  -- write addr + offset back into reg2

-- subtract two registers
eval (Sub (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 - r3)

-- software interrupt
eval (Swi (Con isn))
  = undefined
  --do dbg <- readIORef (debug cpu)
    --   swi cpu isn dbg



----------------------------------------------------------------------
-- Run a CPU until its running flag is set to False.
----------------------------------------------------------------------
run' :: (MonadState CPU m, MonadIO m) => m ()

run' = do singleStep
          run'

singleStep :: (MonadState CPU m, MonadIO m) => m ()
singleStep
  = do pc <- getReg R15
       opcode <- readMem pc
       let instr = decode opcode
       case instr of
         Nothing
           -> do fail ("ERROR: can't decode instruction " ++ (formatHex 8 '0' "" opcode)
                           ++ " at adddress " ++ show pc ++ " (dec)")
         Just instr'
           -> do setReg R15 (pc + 4)
                 eval instr'


runProgram :: MonadIO m => Program -> m CPU
runProgram program = do cpu <- (execStateT (loadProgram program) (CPU emptyMem emptyRegs)) 
                        cpu' <- execStateT run' cpu
                        return cpu'

run :: Program -> IO ()
run program
  = do cpu <- runProgram program
       putStr $ show cpu