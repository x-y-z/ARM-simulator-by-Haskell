{-#OPTIONS  -XFlexibleContexts #-}
module Swi
where

import Data.Bits
import Data.Char
import Data.IORef
import Data.Word

import Control.Monad.State

import CPU
import Loader
import RegisterName

line = "----------------------------------------"

----------------------------------------------------------------------
-- Software interrupt services.
----------------------------------------------------------------------
swi :: (MonadState CPU m, MonadIO m) => Word32 -> Bool -> m ()

-- display character in R0
swi 0 debug
  = do r0 <- getReg R0
       let c = fromIntegral r0
       if debug
         then do liftIO $ putStrLn line
                 liftIO $ putStrLn ("CHR: [" ++ [chr c] ++ "]")
                 liftIO $ putStrLn line
         else liftIO $ putStr [chr c]

-- display integer in R0
swi 1 debug
  = do r0 <- getReg R0
       let r0i = fromIntegral r0
       if debug
         then do liftIO $ putStrLn line
                 liftIO $ putStrLn ("INT: [" ++ show r0i ++ "]")
                 liftIO $ putStrLn line
         else liftIO $ putStr (show r0i)

-- display string starting in location contained in R0
swi 2 debug
  = do r0 <- getReg R0
       str <- fetchString r0
       if debug
         then do liftIO $ putStrLn line
                 liftIO $ putStrLn ("STR: [" ++ str ++ "]")
                 liftIO $ putStrLn line
         else liftIO $ putStr str

-- read a number from the keyboard, place in R0
swi 3 debug
  = do if debug
         then do liftIO $ putStrLn line
                 liftIO $ putStr "INPUT INT: "
         else return ()
       i <- liftIO $ readLn
       if debug
         then liftIO $ putStrLn line
         else return ()
       let w = fromIntegral i
       setReg R0 w

-- read a string from the keyboard, place in buffer that
-- r0 points to, with maximum length in r1 (this service
-- will not store more than r1 - 1 characters in the
-- buffer, the string will automatically be null-terminated)
swi 4 debug
  = do if debug
         then do liftIO $ putStrLn line
                 liftIO $ putStr "INPUT STRING: "
         else return ()
       s <- liftIO $ getLine
       if debug
         then liftIO $ putStrLn s
         else return ()
       addr <- getReg R0
       r1 <- getReg R1
       let len = fromIntegral r1
       let s' = take (len - 1) s
       loadString addr (s' ++ ['\NUL'])

-- display newline
swi 10 debug
  = do if debug
         then do liftIO $ putStrLn line
                 liftIO $ putStrLn "NEWLINE"
                 liftIO $ putStrLn line
         else liftIO $ putStrLn ""

-- exit
swi 11 debug
  = do if debug
         then do liftIO $ putStrLn line
                 liftIO $ putStrLn "NORMAL EXIT"
                 liftIO $ putStrLn line
         else return ()

swi a debug = error $ "unknown SWI: " ++ show a ++ " " ++ show debug

----------------------------------------------------------------------
-- Fetch a string from memory.
----------------------------------------------------------------------
fetchString :: (MonadState CPU m, MonadIO m) => Address -> m String

fetchString addr
  = do word <- readMem addr
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
                              else do s <- fetchString (addr + 4)
                                      return ([chr c1, chr c2, chr c3, chr c4] ++ s)