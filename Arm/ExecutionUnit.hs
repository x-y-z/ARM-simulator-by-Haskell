{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-#OPTIONS  -XFlexibleContexts #-}
module ExecutionUnit
where
  
import Data.Bits

import Control.Monad.State

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic

import CPU
import Instruction
import Memory (Memory (Mem), emptyMem, Segment (DataS))
import Operand
import Register (emptyRegs)
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
       setReg reg1 (r2 Data.Bits..&. r3)

-- branch unconditionally
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
       setReg reg1 (r2 Data.Bits..&. (complement r3))

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
                 _ -> fail "Invalid operand type for Cmp"
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
              
eval (Ldr (Reg reg1) op2)
  = do (bnd,_) <- getBoundM DataS
       val <- case op2 of
                Ind reg2
                  -> do addr <- getReg reg2
                        l <- loadCache (addr + bnd)
                        advanceCycle l
                        readMem (addr + bnd)
                Bas reg2 offset
                  -> do addr <- getReg reg2
                        l <- loadCache (addr + bnd + offset)
                        advanceCycle l
                        readMem (addr + bnd + offset)
                Aut (Bas reg2 offset)
                  -> do addr <- getReg reg2
                        l <- loadCache (addr + bnd + offset)
                        advanceCycle l
                        -- write the address back into reg2     
                        setReg reg2 (addr + bnd + offset)
                        readMem (addr + bnd + offset)
                Pos (Ind reg2) offset
                  -> do addr <- getReg reg2
                        l <- loadCache (addr + bnd + offset)
                        advanceCycle l
                        -- write addr + offset back into reg2
                        setReg reg2 (addr + bnd + offset)  
                        readMem (addr + bnd)
                _ -> fail "Invalid operand type for Load"
       setReg reg1 val         

-- move constant into register
eval (Mov (Reg r) (Con con))
  = setReg r con

-- move register into register
eval (Mov (Reg reg1) (Reg reg2))
  = do val <- getReg reg2
       setReg reg1 val

eval (Mul (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       let prod = (r2 * r3) Data.Bits..&. 0x7FFFFFFF
       setReg reg1 prod

-- logical bit-wise or
eval (Orr (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 .|. r3)

-- store register
eval (Str (Reg reg1) op2)
  = do val <- getReg reg1
       (bnd,_) <- getBoundM DataS
       case op2 of
         Ind reg2
           -> do addr <- getReg reg2
                 writeMem (addr + bnd) val
         Aut (Bas reg2 offset)
           -> do addr <- getReg reg2
                 let addr' = (addr + bnd + offset)
                 writeMem addr' val
                 setReg reg2 addr'  -- write the address back into reg2
         Bas reg2 offset
           -> do addr <- getReg reg2
                 writeMem (addr + bnd + offset) val
         Pos (Ind reg2) offset
           -> do addr <- getReg reg2
                 writeMem (addr + bnd) val
                 -- write addr + offset back into reg2
                 setReg reg2 (addr + bnd + offset)
         _ -> fail "Invalid operand format for Store"

-- subtract two registers
eval (Sub (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 - r3)

-- software interrupt
eval (Swi (Con isn))
 = do db <- isDebug
      swi isn db
      
eval _ = liftIO $ putStrLn "Invalid operand type"

----------------------------------------
-- Unit tests for evaluation
----------------------------------------

testCPU :: CPU
testCPU = CPU (emptyMem []) emptyRegs (D False) emptyCounters_ emptyAux

testEval :: Test
testEval = TestList [ (TestCase tAdd1), 
                      (TestCase tAdd2), 
                      (TestCase tAnd), 
                      (TestCase tB), 
                      (TestCase tBeq), 
                      (TestCase tBic), 
                      (TestCase tBlt),
                      (TestCase tBgt),
                      (TestCase tBne), 
                      (TestCase tCmp1), 
                      (TestCase tCmp2),
                      (TestCase tCmp3),
                      (TestCase tCmp4), 
                      (TestCase tEor), 
                      (TestCase tLdr1),
                      (TestCase tLdr2),
                      (TestCase tLdr3),
                      (TestCase tLdr4),
                      (TestCase tMov1),
                      (TestCase tMov2), 
                      (TestCase tMul),
                      (TestCase tOrr),
                      (TestCase tStr1),
                      (TestCase tStr2),
                      (TestCase tStr3),
                      (TestCase tStr4),
                      (TestCase tSub) ]

tAdd1 :: Assertion
tAdd1 = do cpu <- (execStateT (setReg R1 1) testCPU)
           cpu' <- (execStateT (setReg R2 2) cpu)
           cpu'' <- (execStateT (eval (Add (Reg R0) (Reg R1) (Reg R2))) cpu')
           v <- (evalStateT (getReg R0) cpu'')
           assertEqual "Adding with registers" 3 v
           
tAdd2 :: Assertion
tAdd2 = do cpu <- (execStateT (setReg R1 1) testCPU)
           cpu' <- execStateT (eval (Add (Reg R0) (Reg R1) (Con 2))) cpu
           v <- (evalStateT (getReg R0) cpu')
           assertEqual "Adding a constant" 3 v
           
tAnd :: Assertion
tAnd = do cpu <- (execStateT (setReg R1 0) testCPU)
          cpu' <- (execStateT (setReg R2 127) cpu)
          cpu'' <- (execStateT (eval (And (Reg R0) (Reg R1) (Reg R2))) cpu')
          v <- (evalStateT (getReg R0) cpu'')
          assertEqual "And of two registers" 0 v
          
tB :: Assertion
tB = do cpu <- (execStateT (eval (B (Rel 8))) testCPU)
        v <- (evalStateT (getReg R15) cpu)
        assertEqual "Unconditional branch failed" 4 v

tBeq :: Assertion
tBeq = do cpu <- (execStateT cpsrSetZ testCPU)
          cpu' <- (execStateT (eval (Beq (Rel 8))) cpu)
          v <- (evalStateT (getReg R15) cpu')
          assertEqual "Beq failed" 4 v
          
tBic :: Assertion
tBic = do cpu <- (execStateT (setReg R1 127) testCPU)
          cpu' <- (execStateT (setReg R2 127) cpu)
          cpu'' <- (execStateT (eval (Bic (Reg R0) (Reg R1) (Reg R2))) cpu')
          v <- (evalStateT (getReg R0) cpu'')
          assertEqual "Bit clear failed" 0 v
          
tBlt :: Assertion
tBlt = do cpu <- (execStateT cpsrSetN testCPU)
          cpu' <- (execStateT (eval (Blt (Rel 8))) cpu)
          v <- (evalStateT (getReg R15) cpu')
          assertEqual "Blt failed" 4 v
          
tBgt :: Assertion
tBgt = do cpu <- (execStateT cpsrSetC testCPU)
          cpu' <- (execStateT (eval (Bgt (Rel 8))) cpu)
          v <- (evalStateT (getReg R15) cpu')
          assertEqual "Beq failed" 4 v
          
tBne :: Assertion
tBne = do cpu <- (execStateT (eval (Bne (Rel 8))) testCPU)
          v <- (evalStateT (getReg R15) cpu)
          assertEqual "Bne failed" 4 v
          
tCmp1 :: Assertion
tCmp1 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (setReg R2 5) cpu)
           cpu'' <- (execStateT (eval (Cmp (Reg R1) (Reg R2))) cpu')
           v <- (evalStateT cpsrGetZ cpu'')
           assertEqual "Cmp equality failed" 1 v
           
tCmp2 :: Assertion
tCmp2 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (eval (Cmp (Reg R1) (Con 6))) cpu)
           v <- (evalStateT cpsrGetN cpu')
           assertEqual "Cmp < failed" 1 v
           
tCmp3 :: Assertion
tCmp3 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (eval (Cmp (Reg R1) (Con 4))) cpu)
           v <- (evalStateT cpsrGetC cpu')
           assertEqual "Cmp > failed" 1 v
           
tCmp4 :: Assertion
tCmp4 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (eval (Cmp (Reg R1) (Con 6))) cpu)
           v <- (evalStateT cpsrGetZ cpu')
           assertEqual "Cmp not equal failed" 0 v

tEor :: Assertion
tEor = do cpu <- (execStateT (setReg R1 128) testCPU)
          cpu' <- (execStateT (setReg R2 127) cpu)
          cpu'' <- (execStateT (eval (Eor (Reg R0) (Reg R1) (Reg R2))) cpu')
          v <- (evalStateT (getReg R0) cpu'')
          assertEqual "Exclusive or failed" 255 v
          
tLdr1 :: Assertion          
tLdr1 = do cpu <- (execStateT (setReg R1 4) testCPU)
           cpu' <- (execStateT (writeMem 4 5) cpu)
           cpu'' <- (execStateT (eval (Ldr (Reg R0) (Ind R1))) cpu')
           v <- (evalStateT (getReg R0) cpu'')
           assertEqual "Indirect Load failed" 5 v
           
tLdr2 :: Assertion          
tLdr2 = do cpu <- (execStateT (setReg R1 4) testCPU)
           cpu' <- (execStateT (writeMem 4 5) cpu)
           cpu'' <- (execStateT (eval (Ldr (Reg R0) (Bas R1 0))) cpu')
           v <- (evalStateT (getReg R0) cpu'')
           assertEqual "Offset load failed" 5 v
           
tLdr3 :: Assertion          
tLdr3 = do cpu <- (execStateT (setReg R1 4) testCPU)
           cpu' <- (execStateT (writeMem 4 5) cpu)
           cpu'' <- (execStateT (eval (Ldr (Reg R0) (Aut (Bas R1 0)))) cpu')
           v <- (evalStateT (getReg R0) cpu'')
           assertEqual "Auto load failed" 5 v
           
tLdr4 :: Assertion          
tLdr4 = do cpu <- (execStateT (setReg R1 4) testCPU)
           cpu' <- (execStateT (writeMem 4 5) cpu)
           cpu'' <- (execStateT (eval (Ldr (Reg R0) (Pos (Ind R1) 0))) cpu')
           v <- (evalStateT (getReg R0) cpu'')
           assertEqual "Pos load failed" 5 v

tMov1 :: Assertion
tMov1 = do cpu <- (execStateT (eval (Mov (Reg R0) (Con 5))) testCPU)
           v <- (evalStateT (getReg R0) cpu)
           assertEqual "Mov constant failed" 5 v
           
tMov2 :: Assertion
tMov2 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (eval (Mov (Reg R0) (Reg R1))) cpu)
           v <- (evalStateT (getReg R0) cpu')
           assertEqual "Mov constant failed" 5 v
          
tMul :: Assertion
tMul = do cpu <- (execStateT (setReg R1 5) testCPU)
          cpu' <- (execStateT (setReg R2 6) cpu)
          cpu'' <- (execStateT (eval (Mul (Reg R0) (Reg R1) (Reg R2))) cpu')
          v <- (evalStateT (getReg R0) cpu'')
          assertEqual "Mul failed" 30 v
          
tOrr :: Assertion
tOrr = do cpu <- (execStateT (setReg R1 128) testCPU)
          cpu' <- (execStateT (setReg R2 127) cpu)
          cpu'' <- (execStateT (eval (Orr (Reg R0) (Reg R1) (Reg R2))) cpu')
          v <- (evalStateT (getReg R0) cpu'')
          assertEqual "Orr failed" 255 v
          
tStr1 :: Assertion          
tStr1 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (setReg R2 4) cpu)
           cpu'' <- (execStateT (eval (Str (Reg R1) (Ind R4))) cpu')
           v <- (evalStateT (readMem 0) cpu'')
           assertEqual "Indirect store failed" 5 v
           
tStr2 :: Assertion          
tStr2 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (setReg R2 4) cpu)
           cpu'' <- (execStateT (eval (Str (Reg R1) (Aut (Bas R4 0)))) cpu')
           v <- (evalStateT (readMem 0) cpu'')
           assertEqual "Auto store failed" 5 v
           
tStr3 :: Assertion          
tStr3 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (setReg R2 4) cpu)
           cpu'' <- (execStateT (eval (Str (Reg R1) (Bas R4 0))) cpu')
           v <- (evalStateT (readMem 0) cpu'')
           assertEqual "Offset store failed" 5 v
           
tStr4 :: Assertion          
tStr4 = do cpu <- (execStateT (setReg R1 5) testCPU)
           cpu' <- (execStateT (setReg R2 4) cpu)
           cpu'' <- (execStateT (eval (Str (Reg R1) (Pos (Ind R4) 0))) cpu')
           v <- (evalStateT (readMem 0) cpu'')
           assertEqual "Indirect store failed" 5 v

tSub :: Assertion
tSub = do cpu <- (execStateT (setReg R1 128) testCPU)
          cpu' <- (execStateT (setReg R2 127) cpu)
          cpu'' <- (execStateT (eval (Sub (Reg R0) (Reg R1) (Reg R2))) cpu')
          v <- (evalStateT (getReg R0) cpu'')
          assertEqual "Sub failed" 1 v
          
----------------------------------------
--Evaluation for the in-order pipeline
----------------------------------------

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
       setReg reg1 (r2 Data.Bits..&. r3)

-- branch unconditionally
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
       setReg reg1 (r2 Data.Bits..&. (complement r3))

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
                 _ -> fail "Incorrect operand type for Cmp"
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
  = do (bnd,_) <- getBoundM DataS
       case op2 of
         Ind reg2
           -> do addr <- getReg reg2
                 queueLoad reg1 (addr + bnd)
         Bas reg2 offset
           -> do addr <- getReg reg2
                 queueLoad reg1 (addr + bnd + offset)
         Aut (Bas reg2 offset)
           -> do addr <- getReg reg2
                 setReg reg2 (addr + bnd + offset)
                 queueLoad reg1 (addr + bnd + offset)
         Pos (Ind reg2) offset
           -> do addr <- getReg reg2
                 setReg reg2 (addr + bnd + offset)
                 queueLoad reg1 (addr + bnd)
         _ -> fail "Invalid instruction operand format for Load"
       return ()

-- move constant into register
evalInO (Mov (Reg r) (Con con))
  = setReg r con

-- move register into register
evalInO (Mov (Reg reg1) (Reg reg2))
  = do val <- getReg reg2
       setReg reg1 val

evalInO (Mul (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       let prod = (r2 * r3) Data.Bits..&. 0x7FFFFFFF
       setReg reg1 prod

-- logical bit-wise or
evalInO (Orr (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 .|. r3)

-- store register
evalInO (Str (Reg reg1) op2)
  = do val <- getReg reg1
       (bnd,_) <- getBoundM DataS 
       case op2 of
         Ind reg2
           -> do addr <- getReg reg2
                 queueStore val (addr + bnd)
         Aut (Bas reg2 offset)
           -> do addr <- getReg reg2
                 let addr' = (addr + bnd + offset)
                 queueStore val addr'
                 setReg reg2 addr'
         Bas reg2 offset
           -> do addr <- getReg reg2
                 queueStore val (addr + bnd + offset)
         Pos (Ind reg2) offset
           -> do addr <- getReg reg2
                 queueStore val (addr + bnd)
                 setReg reg2 (addr + bnd + offset)
         _ -> fail "Invalid Operand Type for Store"

-- subtract two registers
evalInO (Sub (Reg reg1) (Reg reg2) (Reg reg3))
  = do r2 <- getReg reg2
       r3 <- getReg reg3
       setReg reg1 (r2 - r3)

-- software interrupt
evalInO (Swi (Con isn))
   = swi isn False
     
evalInO _ = liftIO $ putStrLn "Invalid instruction operand format"

-- evalInO does not write to memory during eval, so we need 
-- to finish these writes/reads when testing random instructions
finishMem :: (MonadState CPU m, MonadIO m) => m ()
finishMem = do st <- getStore
               ld <- getLoad
               case (ld,st) of
                (Just (r,a),Nothing) -> do v <- readMem a
                                           setReg r v
                (Nothing,Just (v,a)) -> do writeMem a v
                (_,_) -> return ()

-- The in-order pipeline must update the PC to 4 less than 
-- the single-stage pipeline, so we need to fix this for
-- random testing
fixPC :: (MonadState CPU m, MonadIO m) => Instruction -> m ()
fixPC i = if isBranch i then 
            do v <- getReg R15
               setReg R15 (v + 4)
            else return ()
           
isBranch :: Instruction -> Bool
isBranch (B _) = True
isBranch (Bne _) = True
isBranch _       = False

checkEvalInstr :: Instruction -> IO Bool
checkEvalInstr i = do cp <- (execStateT (setReg R15 128) testCPU)
                      cp1@(CPU (Mem _ _ m1) r1 _ _ _) <- 
                        (execStateT (eval i) cp)
                      cpu <- (execStateT (evalInO i) cp)
                      cpu' <- (execStateT (fixPC i) cpu)
                      cp2@(CPU (Mem _ _ m2) r2 _ _ _) <-
                        (execStateT finishMem cpu')
                      if (m1 == m2 && r1 == r2) then
                        return True else
                        do liftIO $ putStrLn ((show cp1) ++ "\n\n")
                           liftIO $ putStrLn ((show cp2) ++ "\n\n")
                           return False

prop_eval_eq :: Instruction -> Property
prop_eval_eq i = monadicIO $ do b <- Test.QuickCheck.Monadic.run $ 
                                     checkEvalInstr i
                                Test.QuickCheck.Monadic.assert $ b
                                     