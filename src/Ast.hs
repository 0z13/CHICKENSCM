module Ast where

type Env = [(String, Expr)]

-- Should i make only binary functions possible
-- And then have an implicit fold when we have 
-- over 2 elements in the list? >:)

data Expr 
        = Lit Float 
        | Str String 
        | Id String
        | Plus [Expr]
        | Minus [Expr] 
        | Mult [Expr]  
        | Abs [Expr]
        | Cond Expr Expr Expr
        | TrueC 
        | FalseC
  deriving Show

data LVal 
      = LFloat Float
      | LStr String
      | FunVal Env String Expr
  deriving Show
-- gah

