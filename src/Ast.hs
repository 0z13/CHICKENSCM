module Ast where

type Env = [(String, Expr)]

data Expr 
        = Lit Int
        | Plus Expr Expr
        | Minus Expr Expr 
        | Mult Expr Expr 
  deriving Show
