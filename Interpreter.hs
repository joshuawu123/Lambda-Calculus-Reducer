module Interpreter where
import Data
import Helpers
import Control.Exception
import Debug.Trace
import Data.List

---Contract: normNF_OneStep: Expr -> Maybe Expr
---Purpose: Given an expression E, normNF_OneStep finds the leftmost outermost redex and applies substitution, and returns Just expr', where expr' is the modified expression.
---If there is no redex to be found, it returns Nothing.
---Example: normNF_OneStep (App (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))) (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x")))) should produce Just (App (Lambda "x" (Var "x")) (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))))
---Definition:
normNF_OneStep::Expr -> Maybe Expr
normNF_OneStep e =
    let f = (reduce e) in
        if ((fst f) == Nothing) then Nothing
        else Just (reconstruct e (strip (fst f)) (strip (snd f)))

---Contract: appNF_OneStep: Expr -> Maybe Expr
---Purpose: Given an expression E, appNF_OneStep finds the leftmost innermost redex and applies substitution, and returns Just expr', where expr' is the modified expression.
---If there is no redex to be found, it returns Nothing.
---Example: appNF_OneStep (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))) should produce Just (Lambda "1_" (Lambda "x" (Var "x")))
---Definition:
appNF_OneStep::Expr -> Maybe Expr
appNF_OneStep e = 
    let f = (app_reduce e) in
        if ((fst f) == Nothing) then Nothing
        else Just (reconstruct e (strip (fst f)) (strip (snd f)))

---Contract: normNF: Int -> Expr -> Expr
---Purpose: Given an integer N and an expression E, this will apply appNF_OneStep to E N times.
---Example: normNF 3 (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "z" (App (App (Lambda "x" (Lambda "y" (Var "x"))) (Var "z")) (App (Lambda "x" (App (Var "z") (Var "x"))) (Lambda "x" (App (Var "z") (Var "x"))))))) should produce Lambda "1_" (Lambda "z" (Var "z"))
---Definition:
normNF::Int -> Expr -> Expr
normNF n e = 
    if (n == 0) then e
    else 
        if (hasRedex e) then (normNF (n-1) (strip (normNF_OneStep e)))
        else e

---Contract: appNF: Int -> Expr -> Expr
---Purpose: Given an integer N and an expression E, this will apply appNF_OneStep to E N times.
---Example: appNF 2 (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))) should produce Lambda "1_" (Lambda "x" (Var "x"))
---Definition:
appNF::Int -> Expr -> Expr
appNF n e = 
    if (n == 0) then e
    else 
        if (hasRedex e) then (appNF (n-1) (strip (appNF_OneStep e)))
        else e

---Contract: reduceToNorm: Expr -> Expr
---Purpose: Reduces an expression E to normal form. If a form will be infinitely reduced, for example (\x.xx)(\x.xx), then it will return that expression. 
---Example: reduceToNorm (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))) should produce Lambda "1_" (Lambda "x" (Var "x"))
---Definition:
reduceToNorm::Expr -> Expr
reduceToNorm e = let tmp = (strip (appNF_OneStep e)) in 
    if (tmp == e) then e else if ((hasRedex e) == False) then e else (reduceToNorm tmp)

---Contract: deBruijn: Expr -> Expr 
---Purpose: Converts an expression E to deBruijn form
---Example: deBruijn (Lambda "x" (Lambda "y" (Var "x"))) should produce Lambda "_" (Lambda "_" (Var "2"))
---Definition: 
deBruijn::Expr -> Expr 
deBruijn e = (iterateForward e [])