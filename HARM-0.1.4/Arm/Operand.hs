----------------------------------------------------------------------
-- FILE:              Operand.hs
-- DATE:              02/17/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Operand
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.RegisterName



----------------------------------------------------------------------
-- Operand data type.
----------------------------------------------------------------------
data Operand
  = Aut Operand                 -- auto-increment
  | Bas RegisterName Word32     -- base + offset
  | Con Word32                  -- constant
  | Ind RegisterName            -- indirect
  | Mrg [RegisterName]          -- multiple register
  | Pos Operand Word32          -- post-indexed
  | Reg RegisterName            -- register
  | Rel Int                     -- relative address
  | Lab String                  -- for parsing branches
--  deriving Show


instance Show Operand where
  show (Aut op)      = show op ++ "!"
  show (Bas reg off) = "[" ++ show reg ++ ", #" ++ show off ++ "]"
  show (Con wrd)     = "#" ++ show wrd
  show (Ind reg)     = "[" ++ show reg ++ "]"
  show (Lab lab)     = lab
  show (Mrg regs)    = "{" ++ showMrg regs ++ "}"
  show (Pos op off)  = show op ++ ", #" ++ show off
  show (Reg reg)     = show reg
  show (Rel rel)     = show rel


showMrg []       = ""
showMrg [r]      = show r
showMrg (r : rs) = show r ++ "," ++ showMrg rs



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
