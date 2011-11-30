----------------------------------------------------------------------
-- FILE:              Decoder.hs
-- DATE:              03/05/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Decoder
  ( decode )
where

----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Int
import Data.Maybe
import Debug.Trace
import Data.Word

----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Instruction
import Arm.Operand
import Arm.RegisterName

----------------------------------------------------------------------
-- Decode a word into an instruction.
----------------------------------------------------------------------
decode
  :: Word32
  -> Maybe Instruction

decode word
  = let bits = splitWord word
        bit x = splitWord word (x, x)
        destReg = nthReg (bits (15, 12))
        firstOp = nthReg (bits (19, 16))
        op2 = case (bit 25) of
                0x0  -- register
                  -> Reg (nthReg (bits (3, 0)))
                0x1  -- 8-bit immediate
                  -> Con (bits (7, 0))
    in case bits (27, 24) of
         0xF -> Just (Swi (Con (bits (23, 0))))
         _   -> case bits (27, 25) of
                  0x4 -> decodeMReg word firstOp -- multiple register transfer
                  0x5 -> decodeBranch word
                  _   -> case (bits (27, 26)) of
                           0x0  -- multiplication or data processing instructions
                             -> decodeMulOrDp word destReg firstOp op2
                           0x1  -- data transfer instructions
                             -> decodeDataTrans word destReg
                           _ -> Nothing

----------------------------------------
decodeMulOrDp word destReg firstOp op2
  = let bits = splitWord word
        bit x = splitWord word (x, x)
    in case (bits (27, 24), bit 7, bit 4) of
         (0, 1, 1)
           -> let rm = nthReg (bits (3, 0))
                  rd = nthReg (bits (19, 16))
                  rs = nthReg (bits (11, 8))
              in Just (Mul (Reg rd) (Reg rm) (Reg rs))
         _ -> decodeDataProc (bits (24, 21)) destReg firstOp op2



----------------------------------------
decodeBranch word
  = let link = splitWord word (24, 24)
        offset = fromIntegral (splitWord word (23, 0))
        offset' = if offset > 32767
                    then offset - 65536
                    else offset
             -- if  offset > 8388607 -- this is 2^23 - 1
             --        then offset - 16777216 -- this is  2^24
             --        else offset
            
            
            
            
            -- if offset > 32767
            --         then offset - 65536
            --         else offset
            
            
            
        cond = splitWord word (31, 28)
    in case link of
         0x0
           -> case cond of
                0x0 -> Just (Beq (Rel offset'))
                0x1 -> Just (Bne (Rel offset'))
                0xB -> Just (Blt (Rel offset'))
                0xC -> Just (Bgt (Rel offset'))
                0xE -> Just (B (Rel offset'))
                _   -> Nothing
         0x1
           -> case cond of
                0xE -> Just (Bl (Rel offset'))
                _   -> Nothing

----------------------------------------
decodeDataProc opcode destReg firstOp op2
  = case opcode of
      0x00 -> Just (And (Reg destReg) (Reg firstOp) op2)
      0x01 -> Just (Eor (Reg destReg) (Reg firstOp) op2)
      0x02 -> Just (Sub (Reg destReg) (Reg firstOp) op2)
      0x04 -> Just (Add (Reg destReg) (Reg firstOp) op2)
      0x0A -> Just (Cmp (Reg destReg) op2)
      0x0C -> Just (Orr (Reg destReg) (Reg firstOp) op2)
      0x0D -> Just (Mov (Reg destReg) op2)
      0x0E -> Just (Bic (Reg destReg) (Reg firstOp) op2)
      _    -> Nothing

----------------------------------------
decodeMReg word firstOp
  = let bits = splitWord word
        bit x = splitWord word (x, x)
        instr = case (bit 20) of
                  0x0 -> Ldmea
                  0x1 -> Stmea
        rn = case (bit 21) of
               0x0 -> Reg firstOp
               0x1 -> (Aut (Reg firstOp))
        regList 0 _ = []
        regList n regNum
          | odd n  = (nthReg regNum) : (regList (n `div` 2) (regNum + 1))
          | even n = regList (n `div` 2) (regNum + 1)
        regs = regList (fromIntegral (bits (15, 0))) 0
    in Just (instr rn (Mrg regs))


----------------------------------------
decodeDataTrans word destReg
  = let bits = splitWord word
        bit x = splitWord word (x, x)
        instr = case (bit 22, bit 20) of
                  (0, 0) -> Ldrb
                  (0, 1) -> Strb
                  (1, 0) -> Ldr
                  (1, 1) -> Str
        baseReg = nthReg (bits (19, 16))
        offset = bits (11, 0)
        addrMode = (bit 21) * 2 + bit 24
        op2 = case addrMode of
                0x0 -> if offset == 0
                         then Just (Ind baseReg)
                         else Just (Bas baseReg offset)
                0x1 -> Nothing
                0x2 -> Just (Aut (Bas baseReg offset))
                0x3 -> Just (Pos (Ind baseReg) offset)
     in op2 >>= (\op2' -> Just (instr (Reg destReg) op2'))

----------------------------------------------------------------------
-- Split a word into fields.
----------------------------------------------------------------------
splitWord
  :: Word32
  -> (Int, Int)
  -> Word32

splitWord word (hi, lo)
  = let mask = (2 ^ (hi - lo + 1) - 1) `shiftL` lo
    in (word .&. mask) `shiftR` lo