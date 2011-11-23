module Main where
import System.Environment
import System.IO
import Arm.Arm


main = do
        hSetBuffering stdout NoBuffering
        (s:_) <- getArgs
        putStrLn s
        dbg s