module Intrepreter where 
import Ast

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

