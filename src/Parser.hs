{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Void
import           Data.Text                      ( Text
                                                , pack
                                                )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Functor                   ( void )

import           Instruction

type Parser = Parsec Void Text

parseProgram :: String -> Either (ParseErrorBundle Text Void) [Instruction]
parseProgram = parse program "" . pack

program :: Parser [Instruction]
program = lexeme $ many instruction

instruction :: Parser Instruction
instruction = choice
    [ IncrementPtr <$ symbol ">"
    , DecrementPtr <$ symbol "<"
    , IncrementVal <$ symbol "+"
    , DecrementVal <$ symbol "-"
    , PutChar <$ symbol "."
    , GetChar <$ symbol ","
    , Loop <$> between (symbol "[") (symbol "]") program
    ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space (void $ noneOf ("><+-.,[]" :: String)) empty empty

