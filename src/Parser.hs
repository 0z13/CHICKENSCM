{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified  Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import Ast
import Intrepreter
import Data.Maybe

type Parser = Parsec Void String 

sc :: Parser () 
sc = L.space space1 (L.skipLineComment "//") empty

lexeme = L.lexeme sc 

testStr = "(+ 3 3)" 

op :: Parser Char 
op = choice [char '+', char '-', char '*']

p :: Parser Expr
p = do 
  char '('
  char '+' 
  space
  x <- lexeme L.decimal
  y <- lexeme L.decimal 
  char ')'
  return (Plus (Lit x) (Lit y))

test :: IO ()
test = do
  x <- getLine 
  let y = parseMaybe p x
  putStrLn $ show $ (eval (fromMaybe (Lit 3) y) [("hi", (Lit 3))])
  test
