module Parser
where

import ParseLib
import Data.Word
import Data.Char
import Control.Monad

import BinaryNumber
import CPU
import Memory (Address)
import Instruction
import Operand
import Program
import RegisterName

type Symbol = String



----------------------------------------------------------------------
-- Parse element data type.
----------------------------------------------------------------------
data ParseElement
  = Data        [Operand] [Constant]
  | Instruction Instruction
  | Symbol      Symbol
  | Address     Address
  | Origin      Address
  | RegInit     RegisterName Operand
  | Comment
  | Newline
  deriving (Show)



----------------------------------------------------------------------
-- This parses any number of spaces or tabs.  (``spaces'' parses at
-- least 1 space, and includes all white space characters including \n)
----------------------------------------------------------------------
spaces'
  = many (char ' ' +++ char '\t')



----------------------------------------------------------------------
-- Parse a comma which separates two values.  It can have any number
-- of spaces surrounding it.
----------------------------------------------------------------------
csep
   = do spaces'
        char ','
        spaces'
        return ()

sep c
  = do spaces'
       char c
       spaces'
       return ()



----------------------------------------------------------------------
-- Parse a 32-bit decimal word.
----------------------------------------------------------------------
pWord :: Parser Word32
pWord
  = do { x <- digit; return (fromIntegral (digitToInt x)) }
    `chainl1` return op
  where
    op :: Word32 -> Word32 -> Word32
    m `op` n = 10*m + n



----------------------------------------------------------------------
-- Parse a 32-bit hexadecimal word.
----------------------------------------------------------------------
hexDigit 
  = sat isHexDigit

-- isHexDigit = (`elem` "0123456789abcdefABCDEF")

hexValue '0' = 0
hexValue '1' = 1
hexValue '2' = 2
hexValue '3' = 3
hexValue '4' = 4
hexValue '5' = 5
hexValue '6' = 6
hexValue '7' = 7
hexValue '8' = 8
hexValue '9' = 9
hexValue 'a' = 10
hexValue 'b' = 11
hexValue 'c' = 12
hexValue 'd' = 13
hexValue 'e' = 14
hexValue 'f' = 15
hexValue 'A' = 10
hexValue 'B' = 11
hexValue 'C' = 12
hexValue 'D' = 13
hexValue 'E' = 14
hexValue 'F' = 15

pHex'
  = do { x <- hexDigit; return (hexValue x) }
    `chainl1` return op
  where
    op :: Word32 -> Word32 -> Word32
    m `op` n = 16*m + n

pHex
  = do string "0x"
       pHex'



----------------------------------------------------------------------
-- Parse a binary word.
----------------------------------------------------------------------
pBinary
  :: Parser Word32

pBinary
  = do string "0b"
       bits <- many (char '0' +++ char '1')
       let bn = read bits
       return (binary32ToWord32 bn)



----------------------------------------------------------------------
-- Parse an integer, either hex or decimal.
----------------------------------------------------------------------
pIntegral
  = pHex +++ pBinary +++ pWord



----------------------------------------------------------------------
-- Parse a newline.
----------------------------------------------------------------------
pNl
  = do spaces'
       optional (char '\r')  -- Windows puts a \r before the \n
       char '\n'
       return Newline



-- ====================================================================
-- Header parsers
-- ====================================================================

----------------------------------------------------------------------
-- Parse origin.
----------------------------------------------------------------------
pOrigin
  = do string "origin"
       spaces'
       w <- pIntegral
       return (Origin w)



----------------------------------------------------------------------
-- Parse register initializer.
----------------------------------------------------------------------
pRegInit
  = do spaces'
       string "reg"
       spaces'
       Reg regName <- pReg
       spaces'
       char '='
       spaces'
       o <- pOperand
       spaces'
       return (RegInit regName o)



----------------------------------------------------------------------
-- Parse program header.
----------------------------------------------------------------------
pHeader
  = do o <- pOrigin
       regs <- many pRegInit
       return (o, regs)



----------------------------------------------------------------------
-- Operand parsers.
----------------------------------------------------------------------

-- auto-indexed
pAut :: Parser Operand
pAut
  = do { b <- pBas; char '!'; return (Aut b) }
    +++ do { b <- pReg; char '!'; return (Aut b) }

-- base + offset
pBas :: Parser Operand
pBas
  = do { char '['; (Reg r) <- pReg; csep; Con c <- pCon; char ']'; return (Bas r c) }

-- constant
pCon :: Parser Operand
pCon
  = char '#' >> pIntegral >>= \w -> return (Con w)

-- indirect
pInd :: Parser Operand
pInd
  = do { char '['; Reg r <- pReg; char ']'; return (Ind r) }

-- multiple register
pMrg
  = do char '{'
       regs <- pMrg'
       regs' <- many (do { spaces'; char ','; spaces'; pMrg' })
       char '}'
       return (Mrg (foldl (++) [] (regs : regs')))
  where
    pMrg'
      = pRegRange
        +++ (do Reg r <- pReg
                return [r])
    pRegRange
      = do Reg r1 <- pReg
           char '-'
           Reg r2 <- pReg
           return (enumFromTo r1 r2)

-- post-indexed
pPos :: Parser Operand
pPos
  = do { char '['; Reg r <- pReg; char ']'; csep; Con c <- pCon; 
         return (Pos (Ind r) c) }

-- register
pReg :: Parser Operand
pReg
  = do char 'r'
       i <- nat
       if or [i < 0, i > 15]
         then mzero
         else return (Reg (nthReg (fromIntegral i)))

-- relative offset
pRel
   = do { i <- int; return (Rel i) }

-- parse an operand
pOperand :: Parser Operand
pOperand
  = pAut +++ pBas +++ pCon +++ pPos +++ pInd
    +++ pReg +++ pRel +++ pMrg +++ pBranchLabel



----------------------------------------------------------------------
-- Parse two operands.
----------------------------------------------------------------------
p2Ops
  = do { op1 <- pOperand; csep; op2 <- pOperand; return (op1, op2) }



----------------------------------------------------------------------
-- Parse three operands.
----------------------------------------------------------------------
p3Ops
  = do { op1 <- pOperand; csep; op2 <- pOperand; csep; op3 <- pOperand; 
         return (op1, op2, op3) }



----------------------------------------------------------------------
-- Instruction parsers.
----------------------------------------------------------------------
pAdd   = ops3 "add"   Add
pAnd   = ops3 "and"   And
pB     = ops1 "b"     B
pBeq   = ops1 "beq"   Beq
pBgt   = ops1 "bgt"   Bgt
pBic   = ops3 "bic"   Bic
pBlt   = ops1 "blt"   Blt
pBne   = ops1 "bne"   Bne
pCmp   = ops2 "cmp"   Cmp
pEor   = ops3 "eor"   Eor
pLdr   = ops2 "ldr"   Ldr
pMov   = ops2 "mov"   Mov
pMul   = ops3 "mul"   Mul
pOrr   = ops3 "orr"   Orr
pStr   = ops2 "str"   Str
pSub   = ops3 "sub"   Sub
pSwi   = ops1 "swi"   Swi



----------------------------------------------------------------------
-- Instruction meta-parsers.
----------------------------------------------------------------------
-- instruction with one operand
ops1 name instr
  = do { string name; spaces; op1 <- pOperand; 
         return (Instruction (instr op1)) }

-- instruction with two operands
ops2 name instr
  = do { string name; spaces; (op1, op2) <- p2Ops; 
         return (Instruction (instr op1 op2)) }

-- instruction with three operands
ops3 name instr
  = do { string name; spaces; (op1, op2, op3) <- p3Ops; 
         return (Instruction (instr op1 op2 op3)) }



----------------------------------------------------------------------
-- Parse an instruction.
----------------------------------------------------------------------
pInstr
  = pAdd +++ pAnd +++ pB   +++ pBeq   +++ pBgt   +++ pBic   +++ pBlt
         +++ pBne +++ pCmp +++ pEor   +++ pLdr   +++ pMov
         +++ pMul +++ pOrr +++ pStr   +++ pSub  +++ pSwi
         +++ pLabel



----------------------------------------------------------------------
-- Parse a label.
----------------------------------------------------------------------
pLabel
  = do l <- pLabel'
       char ':'
       return (Symbol l)

pBranchLabel
  = do l <- pLabel'
       return (Lab l)

pLabel'
  = do { xs <- many1 alphanum; return xs }



----------------------------------------------------------------------
-- Parse a comment.
----------------------------------------------------------------------
pComment
  = do { char ';'; many (sat (\x -> x /= '\n')); return Comment }



----------------------------------------------------------------------
-- Return a parsed token in the list monad (optionally ``[]'')
----------------------------------------------------------------------
optional p
  = (do x <- p
        return [x])
    +++ return []



----------------------------------------------------------------------
-- Parse a line of the code segment in a text file.
----------------------------------------------------------------------
pCode
  = (do spaces'
        l <- pLabel
        return l)
    +++ (do spaces'
            i <- pInstr
            return i)
    +++ (do spaces'
            pComment
            return Comment)
    +++ (do char '\n'
            return Newline)



----------------------------------------------------------------------
-- Parse various constants for the data segment.
----------------------------------------------------------------------
pInt
  = int >>= (return . Int)

pChar
  = do char '\''
       c <- sat (\_ -> True)
       char '\''
       return (Int (fromEnum c))

pString
  = do char '"'
       s <- many (sat (\c -> c /= '"'))
       char '"'
       return (String s)

pArray
  = do string "array"
       spaces'
       n <- int
       spaces'
       c <- pData
       return (Array (fromIntegral n) c)



----------------------------------------------------------------------
-- Parse a single constant.
----------------------------------------------------------------------
pData
  = (do w <- pIntegral
        return (Word w))
    +++ pInt
    +++ pChar
    +++ pString
    +++ pArray



----------------------------------------------------------------------
-- Parse a list of constants
----------------------------------------------------------------------
pDataList
  = (do c <- pData
        csep
        cs <- pDataList
        return (c : cs))
    +++ (do c <- pData
            return [c])


----------------------------------------------------------------------
-- Parse a line of the constant segment in a text file.
----------------------------------------------------------------------
pDataLine
  = do label <- optional (do l <- pBranchLabel
                             spaces'
                             char '='
                             return l)
       _     <- spaces'
       cs    <- pDataList
       _     <- optional pComment
       return (Data label cs)



----------------------------------------------------------------------
-- Parse a single program file element.
----------------------------------------------------------------------
pProgElem
  = do spaces'
       elem <- (pNl
                +++ pOrigin
                +++ pRegInit
                +++ pInstr
                +++ pLabel
                +++ pComment
                +++ pDataLine)
       return elem



----------------------------------------------------------------------
-- Parse an entire program.
----------------------------------------------------------------------
pProgram
  = do { elems <- many pProgElem; return elems }


----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
