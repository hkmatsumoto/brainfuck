module Repl where

import           System.Console.Haskeline

import           Control.Monad.State

import           Lib

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        input <- getInputLine ">>> "
        case input of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do
                lift $ exec input
                loop
