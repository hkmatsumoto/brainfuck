module Interpreter where

import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Word                      ( Word8 )
import           Data.Sequence
import           Data.ByteString.Internal       ( c2w
                                                , w2c
                                                )

import           Instruction

data Memory = Memory
    { cells :: Seq Word8
    , ptr :: Int
    }

eval :: Instruction -> StateT Memory IO ()
eval IncrementPtr = modify $ \m -> m { ptr = inc $ ptr m }
eval DecrementPtr = modify $ \m -> m { ptr = dec $ ptr m }
eval IncrementVal = modify $ \m -> m { cells = adjust' inc (ptr m) (cells m) }
eval DecrementVal = modify $ \m -> m { cells = adjust' dec (ptr m) (cells m) }
eval PutChar      = do
    m <- get
    lift . print . w2c $ index (cells m) (ptr m)
eval GetChar = do
    input <- lift $ getChar
    modify $ \m -> m { cells = update (ptr m) (c2w input) (cells m) }
eval (Loop instr) = do
    m <- get
    if (index (cells m) (ptr m)) == 0
        then return ()
        else mapM_ eval instr >> eval (Loop instr)

inc :: Num a => a -> a
inc = (+) 1

dec :: Num a => a -> a
dec = (-) 1
