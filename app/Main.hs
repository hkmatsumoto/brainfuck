module Main where

import           System.Environment             ( getArgs )

import           Lib
import           Repl

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> repl
        _ -> exec =<< readFile (head args)
