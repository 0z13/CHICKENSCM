{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module Parser where
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified  Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import Ast
import Intrepreter
import Data.Maybe
import Control.Applicative hiding (many, some)
import Control.Monad

type Parser = Parsec Void String 

sc :: Parser () 
sc = L.space space1 (L.skipLineComment "//") empty

lexeme = L.lexeme sc 

testStr = "(+ 3 3)" 

op :: Parser Char 
op = choice [char '+', char '-', char '*']


data Sexp 
       = Sym String
       | SNum Float 
       | SList [Sexp]
 deriving Show

pSymbolNoL :: Parser Sexp
pSymbolNoL = Sym <$> 
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")


pOpNoL :: Parser Sexp
pOpNoL = choice [Sym . (:[]) <$> (char '+')
               , Sym . (:[]) <$> (char '-')
               , Sym . (:[]) <$> (char '*')
                ]

pNumNoL :: Parser Sexp
pNumNoL = SNum <$> L.decimal

pItem :: Parser Sexp
pItem = lexeme pSymbolNoL <|> 
        lexeme pOpNoL     <|>
        lexeme pNumNoL

pItemNoLex :: Parser Sexp
pItemNoLex = pSymbolNoL  <|> 
             pOpNoL      <|>
             pNumNoL


pItemList :: Parser Sexp
pItemList = SList <$> pItemNoLex `sepBy` char ' ' 

pParens = between (char '(') (char ')') 

pSymbolList :: Parser Sexp
pSymbolList = pParens pItemList


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
