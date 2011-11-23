module Main where
import Arm.Arm
import System.Environment
import System.IO

main = do
    hSetBuffering stdout NoBuffering
    (s:_) <- getArgs
    putStrLn s
    run s