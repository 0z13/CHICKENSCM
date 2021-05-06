import Test.Tasty
import Test.Tasty.HUnit
import Intrepreter
import Ast
main = defaultMain $ testGroup "(testing tests)" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests:" [
   testCase "Adding:" $
     assertEqual "comparison" (eval  (Plus (Lit 3) (Lit 8)) [("hi", Lit 3)]) 11 

 , testCase "Subtracting:" $
    assertEqual "comparison" (eval  (Minus (Lit 100) (Lit 50)) [("hi", Lit 3)]) 50 
 
    ]


