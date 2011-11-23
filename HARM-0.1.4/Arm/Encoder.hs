----------------------------------------------------------------------
-- FILE:              Encoder.hs
-- DATE:              03/04/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Encoder
  ( encode )
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Int
import Data.Word
import Data.Array



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.Instruction
import Arm.Operand
import Arm.RegisterName



----------------------------------------------------------------------
-- Encoding shortcuts.
----------------------------------------------------------------------
condEq :: (Int, Int, Word32)
condNe :: (Int, Int, Word32)
condCs :: (Int, Int, Word32)
condHs :: (Int, Int, Word32)
condCc :: (Int, Int, Word32)
condLo :: (Int, Int, Word32)
condMi :: (Int, Int, Word32)
condPl :: (Int, Int, Word32)
condVs :: (Int, Int, Word32)
condVc :: (Int, Int, Word32)
condHi :: (Int, Int, Word32)
condLs :: (Int, Int, Word32)
condGe :: (Int, Int, Word32)
condLt :: (Int, Int, Word32)
condGt :: (Int, Int, Word32)
condLe :: (Int, Int, Word32)
condAl :: (Int, Int, Word32)
condNv :: (Int, Int, Word32)

condEq = (31, 28, 0x0)
condNe = (31, 28, 0x1)
condCs = (31, 28, 0x2)
condHs = (31, 28, 0x2)
condCc = (31, 28, 0x3)
condLo = (31, 28, 0x3)
condMi = (31, 28, 0x4)
condPl = (31, 28, 0x5)
condVs = (31, 28, 0x6)
condVc = (31, 28, 0x7)
condHi = (31, 28, 0x8)
condLs = (31, 28, 0x9)
condGe = (31, 28, 0xA)
condLt = (31, 28, 0xB)
condGt = (31, 28, 0xC)
condLe = (31, 28, 0xD)
condAl = (31, 28, 0xE)
condNv = (31, 28, 0xF)



----------------------------------------------------------------------
-- Encode an instruction into a Word32.
----------------------------------------------------------------------
encode
  :: Instruction
  -> Word32

----------------------------------------
-- add three registers
encode (Add (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x04)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(25, 25, 0), (3, 0, regIndex r3)]   -- second operand is register
                  Con c1
                    -> [(25, 25, 1), (7, 0, c1)]            -- 8-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- logical bit-wise and
encode (And (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x00)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                    -> [(7, 0, c1)]            -- 8-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- branch unconditionally
encode (B (Rel rel))
  = encodeBranch condAl rel

----------------------------------------
-- branch if equal
encode (Beq (Rel rel))
  = encodeBranch condEq rel

----------------------------------------
-- branch if greater than
encode (Bgt (Rel rel))
  = encodeBranch condGt rel

----------------------------------------
-- bit clear
encode (Bic (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x0E)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                    -> [(7, 0, c1)]            -- 8-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- branch and link
encode (Bl (Rel rel))
  = encodeBranch condAl rel .|. concatFields 0 [(24, 24, 1)]

----------------------------------------
-- branch if less than
encode (Blt (Rel rel))
  = encodeBranch condLt rel

----------------------------------------
-- branch if not equal
encode (Bne (Rel rel))
  = encodeBranch condNe rel

----------------------------------------
-- compare two operands
encode (Cmp (Reg r1) op2)
  = let w1 = concatFields 0
               [ condAl                  -- condition
               , (24, 21, 0x0A)          -- opcode
               , (15, 12, regIndex r1)   -- register 1
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

----------------------------------------
-- logical bit-wise exclusive or
encode (Eor (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x01)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                    -> [(7, 0, c1)]            -- 8-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- load multiple registers
encode (Ldmea op1 (Mrg regs))
  = encodeMReg 0x0 op1 regs

----------------------------------------
-- load register
encode (Ldr (Reg r1) op2)
  = encodeLdrStr 0x0 0x1 r1 op2

----------------------------------------
-- load register, unsigned byte
encode (Ldrb (Reg r1) op2)
  = encodeLdrStr 0x0 0x0 r1 op2

----------------------------------------
-- move register to register
encode (Mov (Reg r1) op2)
  = let w1 = concatFields 0
               [ condAl                  -- condition
               , (24, 21, 0x0D)          -- opcode
               , (15, 12, regIndex r1)   -- destination register
               ]
        w2 = encodeOp2 op2
    in w1 .|. w2

----------------------------------------
-- multiply
encode (Mul (Reg r1) (Reg r2) (Reg r3))
  = concatFields 0
      [ condAl
      , (19, 16, regIndex r1)
      , (11,  8, regIndex r3)
      , ( 7,  4, 0x09)
      , ( 3,  0, regIndex r2)
      ]

----------------------------------------
-- logical bit-wise or
encode (Orr (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x0C)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                    -> [(7, 0, c1)]            -- 8-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- load multiple registers
encode (Stmea op1 (Mrg regs))
  = encodeMReg 0x1 op1 regs

----------------------------------------
-- store register
encode (Str (Reg r1) op2)
  = encodeLdrStr 0x1 0x1 r1 op2

----------------------------------------
-- store register, unsigned byte
encode (Strb (Reg r1) op2)
  = encodeLdrStr 0x1 0x0 r1 op2

----------------------------------------
-- add three registers
encode (Sub (Reg r1) (Reg r2) op2)
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (24, 21, 0x02)         -- opcode
               , (19, 16, regIndex r2)  -- first operand register
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Reg r3
                    -> [(3, 0, regIndex r3)]   -- second operand register
                  Con c1
                    -> [(7, 0, c1)]            -- 8-bit constant
               )
    in w1 .|. w2

----------------------------------------
-- software interrupt
encode (Swi (Con c))
  = concatFields 0 [ condAl
                   , (27, 24, 0xF)
                   ] .|. c

----------------------------------------------------------------------
-- helper functions

encodeBranch cond rel
  = concatFields 0 [ cond,
  (27, 25, 0x5)
  ] .|. (to16to32 rel)


to16to32 n = (fromIntegral (fromIntegral n :: Word16) :: Word32)


encodeOp2 op
  = concatFields 0
      (case op of
        Reg r2
          -> [(3, 0, regIndex r2)]    -- first operand register
        Con c1
          -> [ (25, 25, 0x01)         -- ``#'' field
             , (7, 0, c1)             -- 8-bit immediate
             ])

-- encode a multiple register load or store
encodeMReg ls op1 regs
  = let w1 = concatFields 0
               [ condAl
               , (27, 25, 0x04)    -- opcode
             --, (24, 24, 0x00)    -- post-increment or decrement
               , (23, 23, 0x01)    -- increment or decrement
               , (20, 20, ls)      -- load
               ]
        w2 = concatFields 0
               (case op1 of
                  Aut (Reg reg)
                    -> [ (21, 21, 0x01)   -- write-back
                       , (19, 16, regIndex reg)
                       ]
                  Reg reg
                    -> [ (19, 16, regIndex reg)
                       ]
               )
        w3 = concatFields 0
               (map (\reg -> let i = fromIntegral (regIndex reg) in (i, i, 1)) regs)
    in w1 .|. w2 .|. w3

-- encode a load or store
encodeLdrStr ls bw r1 op2
  = let w1 = concatFields 0
               [ condAl                 -- condition
               , (27, 26, 0x01)         -- constant field
             --, (25, 25, 0x00)         -- ``#'' field
             --, (23, 23, 0x00)         -- up/down
               , (22, 22, bw)           -- unsigned byte/word
               , (20, 20, ls)           -- load/store
               , (15, 12, regIndex r1)  -- destination register
               ]
        w2 = concatFields 0
               (case op2 of
                  Ind r2
                    -> [ --(24, 24, 0x00)        -- pre/post index
                     --, (21, 21, 0x00)        -- write-back (auto-index)
                         (19, 16, regIndex r2) -- base register
                       ]
                  Bas r2 offset
                    -> [ --(24, 24, 0x00)        -- pre/post index
                     --, (21, 21, 0x00)        -- write-back (auto-index)
                         (19, 16, regIndex r2) -- base register
                       , (11,  0, offset)      -- offset
                       ]
                  Aut (Bas r2 offset)
                    -> [ --(24, 24, 0x00)        -- pre/post index
                         (21, 21, 0x01)        -- write-back (auto-index)
                       , (19, 16, regIndex r2) -- base register
                       , (11,  0, offset)      -- offset
                       ]
                  Pos (Ind r2) const
                    -> [ (24, 24, 0x01)        -- pre/post index
                       , (21, 21, 0x01)        -- write-back (auto-index)
                       , (19, 16, regIndex r2) -- base register
                       , (11,  0, const)       -- offset
                       ]
               )
    in w1 .|. w2




----------------------------------------------------------------------
-- Concatenate bit fields into one word.
----------------------------------------------------------------------
concatFields
  :: Word32
  -> [(Int, Int, Word32)]
  -> Word32

concatFields word [] = word
concatFields word ((hi, lo, val) : fields)
  = let mask = fromIntegral (2 ^ (hi - lo + 1) - 1)
        val' = val .&. mask
    in concatFields (word .|. (val' `shiftL` lo)) fields



----------------------------------------------------------------------
-- Convert a register name into a word32.
----------------------------------------------------------------------
regIndex
  :: RegisterName
  -> Word32

regIndex = fromIntegral . (index (R0, CPSR))



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
