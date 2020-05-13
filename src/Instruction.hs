module Instruction where

data Instruction
    = IncrementPtr
    | DecrementPtr
    | IncrementVal
    | DecrementVal
    | PutChar
    | GetChar
    | Loop [Instruction]
