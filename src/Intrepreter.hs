module Intrepreter where 
import Ast

{-
eval :: Expr -> Env -> Int
eval (Lit n) env        = n
eval (Plus x y) env     = let x' = eval x env 
                              y' = eval y env
                              in x' + y'
eval (Mult x y) env     = let x' = eval x env 
                              y' = eval y env
                              in x' * y' 
eval (Minus x y) env     = let x' = eval x env 
                               y' = eval y env
                               in x' - y'
-}

-- I think i will do this with monad transformers tomorrow!
-- We need TYPES 
eval :: Expr -> Float
eval (Lit n) = n
eval (Plus xs) = sum $ (map eval) xs
eval _       = eval $ (Lit 3.0)
