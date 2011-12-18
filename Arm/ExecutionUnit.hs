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
import Swi

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
         then do setReg R15 pc''
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
         then do setReg R15 pc''
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
         then do setReg R15 pc''
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

-- subtract two registers
eval (Sub (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 - r3)

-- software interrupt
eval (Swi (Con isn))
 = do liftIO $ putStrLn "here"
      cpu <- get
      --dbg <- isDebug
      swi isn False


evalInO :: (MonadState CPU m, MonadIO m) => Instruction -> m ()

-- add two registers
evalInO (Add (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 + r3)

evalInO (Add (Reg reg1) (Reg reg2) (Con con1))
  = do r2 <- getReg reg2
       setReg reg1 (r2 + con1)

-- logical bit-wise and
evalInO (And (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 .&. r3)

-- branch unconditionally
-- TODO:  Change this to use actual PC rather than R15
evalInO (B (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 8
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       setReg R15 pc''
       flushPipeline

-- branch if equal
evalInO (Beq (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 8
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       z <- cpsrGetZ
       if z == 1
         then do {setReg R15 pc''; flushPipeline}
         else return ()

-- branch if greater than
evalInO (Bgt (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 8
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       c <- cpsrGetC
       if c == 1
         then do {setReg R15 pc''; flushPipeline}
         else return ()

-- bit clear
evalInO (Bic (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 .&. (complement r3))

-- branch and link
evalInO (Bl (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 8
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       setReg R14 pc
       setReg R15 pc''
       flushPipeline

-- branch if less than
evalInO (Blt (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 8
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       n <- cpsrGetN
       if n == 1
         then do {setReg R15 pc''; flushPipeline}
         else return ()

-- branch if not equal
evalInO (Bne (Rel offset))
  = do pc <- getReg R15
       let pc' = pc - 8
       let pc'' = if offset < 0
                    then pc' - (fromIntegral (-offset))
                    else pc' + (fromIntegral offset)
       z <- cpsrGetZ
       if z == 0
         then do {setReg R15 pc''; flushPipeline}
         else return ()

-- compare two values
evalInO (Cmp (Reg reg1) op2)
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
evalInO (Eor (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 `xor` r3)
              
-- load register
evalInO (Ldr (Reg reg1) op2)
  = do addr <- case op2 of
                Ind reg2
                  -> do addr <- getReg reg2
                        queueLoad reg1 addr
                Bas reg2 offset
                  -> do addr <- getReg reg2
                        queueLoad reg1 (addr + offset)
                Aut (Bas reg2 offset)
                  -> do addr <- getReg reg2
                        setReg reg2 (addr + offset)
                        queueLoad reg1 (addr + offset)
                Pos (Ind reg2) offset
                  -> do addr <- getReg reg2
                        setReg reg2 (addr + offset)
                        queueLoad reg1 addr
       return ()

-- move constant into register
evalInO (Mov (Reg reg) (Con con))
  = setReg reg con

-- move register into register
evalInO (Mov (Reg reg1) (Reg reg2))
  = do val <- getReg reg2
       setReg reg1 val

evalInO (Mul (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       let prod = (r2 * r3) .&. 0x7FFFFFFF
       setReg reg1 prod

-- logical bit-wise or
evalInO (Orr (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 .|. r3)

-- store register
evalInO (Str (Reg reg1) op2)
  = do case op2 of
         Ind reg2
           -> do addr <- getReg reg2
                 queueStore reg1 addr
         Aut (Bas reg2 offset)
           -> do addr <- getReg reg2
                 let addr' = addr + offset
                 queueStore reg1 addr'
                 setReg reg2 addr'
         Bas reg2 offset
           -> do addr <- getReg reg2
                 queueStore reg1 (addr + offset)
         Pos (Ind reg2) offset
           -> do addr <- getReg reg2
                 queueStore reg1 addr
                 setReg reg2 (addr + offset)

-- subtract two registers
evalInO (Sub (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 - r3)

-- software interrupt
evalInO (Swi (Con isn))
   = swi isn False
