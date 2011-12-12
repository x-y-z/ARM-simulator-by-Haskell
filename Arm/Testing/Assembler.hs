module Assembler
where

import Prelude
import Data.Word
import Data.Char

import Instruction
import Operand
import ParseLib
import Parser
import Program
import RegisterName

data AsmResult
  = Res Program
  | Err String
  deriving (Show)

expandMacros l = l

resolveSymbols
  :: Word32
  -> [ParseElement]
  -> [(String, Word32)]

resolveSymbols _ []
  = []

resolveSymbols addr (Origin org : rest)
  = resolveSymbols org rest

resolveSymbols addr (Symbol l : rest)
  = (l, addr) : resolveSymbols addr rest

resolveSymbols addr (Instruction _ : rest)
  = resolveSymbols (addr + 4) rest

resolveSymbols addr (Data [] ds : rest)
  = let dSize = constSize (List ds)
    in resolveSymbols (addr + dSize) rest

resolveSymbols addr (Data [Lab l] ds : rest)
  = let dSize = constSize (List ds)
    in (l, addr) : resolveSymbols (addr + dSize) rest

resolveSymbols addr (_ : rest)
  = resolveSymbols addr rest



----------------------------------------------------------------------
-- Replace symbols with addresses.
----------------------------------------------------------------------
replaceSymbols
  :: [ParseElement]            -- elements being parsed
  -> Int                       -- current line number in source file
  -> Word32                    -- current address in memory
  -> [(String, Word32)]        -- table of labels
  -> Word32                    -- origin
  -> [(RegisterName, Word32)]  -- initial register bindings
  -> [Instruction]             -- instruction accumulator list
  -> [(Word32, Constant)]      -- constant accumulator list
  -> Program

----------
replaceSymbols [] line addr _ origin regBindings iAccum cAccum
  = Program
      { memorySize = addr
      , origin = origin
      , regInit = reverse regBindings
      , instructions = reverse iAccum
      , constants = reverse cAccum
      }

----------
replaceSymbols (Origin org : rest) line addr lTab origin regBindings iAccum cAccum
  = replaceSymbols rest line org lTab org regBindings iAccum cAccum

----------
replaceSymbols (Instruction i : rest) line addr lTab origin regBindings iAccum cAccum
  = let i' = case i of
               B   (Lab l) -> replaceBranch B   lTab addr line l
               Beq (Lab l) -> replaceBranch Beq lTab addr line l
               Bgt (Lab l) -> replaceBranch Bgt lTab addr line l
               Bl  (Lab l) -> replaceBranch Bl  lTab addr line l
               Blt (Lab l) -> replaceBranch Blt lTab addr line l
               Bne (Lab l) -> replaceBranch Bne lTab addr line l
               _           -> i
    in replaceSymbols rest line (addr + 4) lTab origin regBindings (i' : iAccum) cAccum

----------
replaceSymbols (RegInit regName op : rest) line addr lTab origin regBindings iAccum cAccum
  = let val = case op of
                Lab label
                  -> case lookup label lTab of
                       Nothing
                         -> error ("label " ++ label ++ " does not exist, line " ++ show line)
                       Just label'
                         -> label'
    in replaceSymbols rest line addr lTab origin ((regName, val) : regBindings) iAccum cAccum

----------
replaceSymbols (Newline : rest) line addr lTab origin regBindings iAccum cAccum
  = replaceSymbols rest (line + 1) addr lTab origin regBindings iAccum cAccum

----------
replaceSymbols (Data [] data' : rest) line addr lTab origin regBindings iAccum cAccum
  = let c = case data' of
              [c']
                -> c'
              _ -> List data'
        size = constSize c
    in replaceSymbols rest line (addr + size) lTab origin regBindings iAccum ((addr, c) : cAccum)

----------
replaceSymbols (Data [Lab label] data' : rest) line addr lTab origin regBindings iAccum cAccum
  = let c = case data' of
              [c']
                -> c'
              _ -> List data'
        size = constSize c
        addr' = case lookup label lTab of
                  Nothing
                    -> error ("label " ++ label ++ " does not exist, line " ++ show line)
                  Just addr''
                    -> addr''
    in replaceSymbols rest line (addr + size) lTab origin regBindings iAccum ((addr', c) : cAccum)

----------
replaceSymbols (_ : rest) line addr lTab origin regBindings iAccum cAccum
  = replaceSymbols rest line addr lTab origin regBindings iAccum cAccum



----------------------------------------------------------------------
-- 
----------------------------------------------------------------------
replaceBranch branchInstruction lTab addr line label
  = let a = lookup label lTab
    in case a of
         Nothing
           -> error ("label " ++ label ++ " not bound, line " ++ show line)
         Just addr'
           -> branchInstruction (Rel (fromIntegral addr' - fromIntegral addr))



----------------------------------------------------------------------
-- Assemble a program text string into a program.
----------------------------------------------------------------------
asmString
  :: String
  -> Either Program String

asmString progString
  = let prog = papply pProgram progString
    in case prog of
         ((prog', "") : _)
           -> let lTab = resolveSymbols 0 prog'
              in Left (replaceSymbols prog' 1 0 lTab 0 [] [] [])
         ((prog', str) : _)
           -> Right (errorMessage prog' str)



----------------------------------------------------------------------
-- Generate an error message.
----------------------------------------------------------------------
errorMessage prog' remainingInput
  = let lines = countLines prog' 1
        errLine = dropWhile isSpace (head (lines' remainingInput))
    in ("error, line " ++ show lines ++ ": " ++ errLine)
  where
    countLines [] accum
      = accum
    countLines (Newline : rest) accum
      = countLines rest (accum + 1)
    countLines (_ : rest) accum
      = countLines rest accum


lines'
  :: String
  -> [String]

lines' ""
  = []
lines' s
  = let (l,s') = break (\x -> or [x == '\n', x == '\r']) s
    in l : case s' of
             []
               -> []
             (_:s'')
               -> lines' s''


----------------------------------------------------------------------
-- Assemble a text file into a program.
----------------------------------------------------------------------
asmFile
  :: String
  -> IO (Either Program String)

asmFile fileName
  = do file <- readFile fileName
       let progOrError = asmString file
       return progOrError