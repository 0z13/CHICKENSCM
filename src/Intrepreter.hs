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
--
-- NVM transformers another time 
eval :: Expr -> LVal 
eval (Lit n) = LFloat n 
eval (Plus xs) =  primAdd xs (LFloat 0)
eval (Mult xs) =  primMult xs (LFloat 1)
eval (Minus xs) = primMinus xs (LFloat 0)
eval _       = LFloat 3.0

primMinus :: [Expr] -> LVal -> LVal
primMinus [] r           = r 
primMinus ((Lit x):xs) (LFloat r) = primAdd xs (LFloat $ r-x)
primMinus ((Str x):xs) (LFloat r) = error "Minusing strings? What do YOU suggest should happen?" 

primAdd :: [Expr] -> LVal -> LVal
primAdd [] r           = r 
primAdd ((Str x):xs) (LFloat r) = error "Adding strings? What do YOU suggest should happen?" 
primAdd ((Lit x):xs) (LFloat r) = primMult xs (LFloat $ r+x)

primMult :: [Expr] -> LVal -> LVal
primMult [] r           = r 
primMult ((Str x):xs) (LFloat r) = error "Multiplying strings? What do YOU suggest should happen?" 
primMult ((Lit x):xs) (LFloat r) = primMult xs (LFloat $ r*x)
