module Interpreter where

import           Control.Monad.State

import           Data.Word                      ( Word8 )
import           Data.Sequence
import qualified Data.Sequence                 as S
import           Data.ByteString.Internal       ( c2w
                                                , w2c
                                                )

import           Instruction

data Memory = Memory
    { cells :: Seq Word8
    , ptr :: Int
    }

run :: [Instruction] -> IO ()
run instr = evalStateT (eval' instr) newMemory
    where newMemory = Memory { cells = S.replicate 10 0, ptr = 0 }

eval :: Instruction -> StateT Memory IO ()
eval IncrementPtr = modify $ \m -> m { ptr = inc $ ptr m }
eval DecrementPtr = modify $ \m -> m { ptr = dec $ ptr m }
eval IncrementVal = modify $ \m -> m { cells = adjust' inc (ptr m) (cells m) }
eval DecrementVal = modify $ \m -> m { cells = adjust' dec (ptr m) (cells m) }
eval PutChar      = do
    m <- get
    lift . putStr . (: []) . w2c $ index (cells m) (ptr m)
eval GetChar = do
    input <- lift $ getChar
    modify $ \m -> m { cells = update (ptr m) (c2w input) (cells m) }
eval (Loop instr) = do
    m <- get
    if index (cells m) (ptr m) == 0
        then return ()
        else eval' instr >> eval (Loop instr)

eval' :: [Instruction] -> StateT Memory IO ()
eval' = mapM_ eval

inc :: Num a => a -> a
inc = flip (+) 1

dec :: Num a => a -> a
dec = flip (-) 1
