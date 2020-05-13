module Lib where

import           Text.Megaparsec

import           Interpreter
import           Parser

exec :: String -> IO ()
exec code = case parseProgram code of
    Left  err   -> print $ errorBundlePretty err
    Right instr -> run instr
