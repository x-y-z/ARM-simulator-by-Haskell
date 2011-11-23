----------------------------------------------------------------------
-- FILE:              Swi.hs
-- DESCRIPTION:       
-- DATE:              03/22/2001
-- PROJECT:           
-- LANGUAGE PLATFORM: 
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Swi
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
import Data.Bits
import Data.Char
import Data.IORef
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------
import Arm.CPU
import Arm.Loader
import Arm.Memory
import Arm.Register
import Arm.RegisterName



line = "----------------------------------------"

----------------------------------------------------------------------
-- Software interrupt services.
----------------------------------------------------------------------
swi
  :: CPU
  -> Word32
  -> Bool
  -> IO ()

-- display character in R0
swi cpu 0 debug
  = do let regs = registers cpu
       r0 <- getReg regs R0
       let c = fromIntegral r0
       if debug
         then do putStrLn line
                 putStrLn ("CHR: [" ++ [chr c] ++ "]")
                 putStrLn line
         else putStr [chr c]

-- display integer in R0
swi cpu 1 debug
  = do let regs = registers cpu
       r0 <- getReg regs R0
       let r0i = fromIntegral r0
       if debug
         then do putStrLn line
                 putStrLn ("INT: [" ++ show r0i ++ "]")
                 putStrLn line
         else putStr (show r0i)

-- display string starting in location contained in R0
swi cpu 2 debug
  = do let regs = registers cpu
       r0 <- getReg regs R0
       str <- fetchString (memory cpu) r0
       if debug
         then do putStrLn line
                 putStrLn ("STR: [" ++ str ++ "]")
                 putStrLn line
         else putStr str

-- read a number from the keyboard, place in R0
swi cpu 3 debug
  = do if debug
         then do putStrLn line
                 putStr "INPUT INT: "
         else return ()
       i <- readLn
       if debug
         then putStrLn line
         else return ()
       let w = fromIntegral i
       let regs = registers cpu
       setReg regs R0 w

-- read a string from the keyboard, place in buffer that
-- r0 points to, with maximum length in r1 (this service
-- will not store more than r1 - 1 characters in the
-- buffer, the string will automatically be null-terminated)
swi cpu 4 debug
  = do if debug
         then do putStrLn line
                 putStr "INPUT STRING: "
         else return ()
       s <- getLine
       if debug
         then putStrLn s
         else return ()
       let regs = registers cpu
       addr <- getReg regs R0
       r1 <- getReg regs R1
       let len = fromIntegral r1
       let s' = take (len - 1) s
       let mem = memory cpu
       loadString mem addr (s' ++ ['\NUL'])

-- display newline
swi cpu 10 debug
  = do if debug
         then do putStrLn line
                 putStrLn "NEWLINE"
                 putStrLn line
         else putStrLn ""

-- exit
swi cpu 11 debug
  = do if debug
         then do putStrLn line
                 putStrLn "NORMAL EXIT"
                 putStrLn line
         else return ()
       let runFlag = running cpu
       writeIORef runFlag False

swi cpu a debug = error $ "unknown SWI: " ++ show a ++ " " ++ show debug

----------------------------------------------------------------------
-- Fetch a string from memory.
----------------------------------------------------------------------
fetchString
  :: Memory
  -> Address
  -> IO String

fetchString mem addr
  = do word <- readMem mem addr
       let c4 = fromIntegral ((word .&. 0xFF000000) `shift` (-24))
       let c3 = fromIntegral ((word .&. 0xFF0000) `shift` (-16))
       let c2 = fromIntegral ((word .&. 0xFF00) `shift` (-8))
       let c1 = fromIntegral (word .&. 0xFF)
       if c1 == 0
         then return ""
         else if c2 == 0
                then return [chr c1]
                else if c3 == 0
                       then return [chr c1, chr c2]
                       else if c4 == 0
                              then return [chr c1, chr c2, chr c3]
                              else do s <- fetchString mem (addr + 4)
                                      return ([chr c1, chr c2, chr c3, chr c4] ++ s)




----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
