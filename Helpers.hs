module Helpers where
import Data
import Data.List


---Contract: freeVars: Expr -> [Name]
---Purpose: Calculate the list of free variables given an input expression.
---Example: freeVars (Lambda "x" (App (Var "x") (Var "y"))) should produce ["y"]
---Definition:
freeVars::Expr -> [Name]
freeVars e = 
    case e of 
        Var n -> [n]
        Lambda n e1 -> (freeVars e1) \\ [n]
        App e1 e2 -> (union (freeVars e1) (freeVars e2))

---Contract: freshVars: [Expr]->[Name]
---Purpose: Generate a list of free variables to use, which consists [1..] minus any free variables in the expression, and adds an underscore to all of the values
---Example: freshVars [(Var "1_")] should produce [2_,3_,4_,..]
---Definition: 
freshVars::[Expr]->[Name]
freshVars expr_li = (map addUnderscore [1..]) \\ (concat (map freeVars expr_li))

---Contract: addUnderscore: Int -> Name
---Purpose: Adds an underscore after the input integer.
---Example: addUnderscore 1 should produce "1_"
---Definition:
addUnderscore::Int -> Name
addUnderscore v = (show v) ++ "_"

---Contract: subst: (Name, Expr) -> Expr -> Expr
---Purpose: Given an expression E, another expression M, and a variable to replace x, E[M/x] replaces all free occurrences of x in E by M.
---Example: subst ("x", (Var "y")) (Lambda "x" (App (Var "x") (Var "y"))) should produce Lambda "1_" (App (Var "1_") (Var "y"))
---Definition:
subst::(Name,Expr) -> Expr -> Expr
subst m e =
    case e of
        Var n -> if (fst m) == n then (snd m) else (Var n)
        App e1 e2 -> (App (subst m e1) (subst m e2))
        Lambda n e1 ->
             let f = (subst (n, (Var (head (freshVars [e1,(snd m)])))) e1) in 
                        (Lambda (head (freshVars [e1,(snd m)])) (subst m f))

---Contract: reduce: Expr -> (Maybe Expr, Maybe Expr)
---Purpose: Given an expression E, reduce finds the leftmost innermost redex and applies substitution, and returns a tuple of (Just e', Just expr'), 
---where e' is the expression to be modified and expr' is the expression after substitution is performed on it. If there is no redex to be found, it returns (Nothing, Nothing).
---Example: reduce (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))) should produce (Just (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))),Just (Lambda "1_" (Lambda "x" (Var "x"))))
---Definition:
reduce::Expr -> (Maybe Expr, Maybe Expr)
reduce e = 
    case e of 
        Var n -> (Nothing, Nothing)
        Lambda n e1 -> (reduce e1)
        App e1 e2 -> 
            case e1 of 
                Var n1 -> if (hasRedex e2 == True)
                    then (reduce e2) 
                    else (Nothing, Nothing)
                App e11 e21 -> if (hasRedex e1 == True)
                    then (reduce e1)
                    else 
                        if (hasRedex e2 == True)
                            then (reduce e2)
                            else (Nothing,Nothing)
                Lambda n1 e11 -> (Just e, Just (subst (n1, e2) e11))

---Contract: app_reduce: Expr -> (Maybe Expr, Maybe Expr)
---Purpose: Given an expression E, app_reduce finds the leftmost innermost redex and applies substitution, and returns a tuple of (Just e', Just expr'), 
---where e' is the expression to be modified and expr' is the expression after substitution is performed on it. If there is no redex to be found, it returns (Nothing, Nothing).
---Example: app_reduce (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))) should produce (Just (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))),Just (Lambda "1_" (Lambda "x" (Var "x"))))
---Definition:
app_reduce::Expr -> (Maybe Expr, Maybe Expr)
app_reduce e = 
    case e of 
        Var n -> (Nothing, Nothing)
        Lambda n e1 -> (app_reduce e1)
        App e1 e2 -> 
            if (hasRedex e1 == True)
                then (app_reduce e1) 
                else
                    if (hasRedex e2 == True)
                    then (app_reduce e2)
                    else 
                        case e1 of 
                            Var n1 -> (Nothing, Nothing)
                            App e11 e21 -> (Nothing, Nothing)
                            Lambda n1 e11 -> (Just e, Just (subst (n1, e2) e11))

---Contract: reconstruct: Expr -> Maybe Expr -> Maybe Expr -> Expr
---Purpose: Given an expression E, an inner expression to replace R, and an expression to replace it with S, it will replace the leftmost occurrence of R with S 
---Example: reconstruct (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))) (Just (App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x")))) (Just (Lambda "1_" (Lambda "x" (Var "x")))) should produce Lambda "1_" (Lambda "x" (Var "x"))
---Definition:
reconstruct::Expr -> Expr -> Expr -> Expr
reconstruct e toReplace toSubIn =
    case e of 
        Var n -> if (e == toReplace) then toSubIn else (Var n)
        Lambda n e1 -> if (e == toReplace) then toSubIn else (Lambda n (reconstruct e1 toReplace toSubIn))
        App e1 e2 -> if (e == toReplace) then toSubIn else 
            if (hasExpr e1 toReplace == True) then 
                (App (reconstruct e1 toReplace toSubIn) e2)
                else 
                    (App e1 (reconstruct e2 toReplace toSubIn))

---Contract: hasExpr: Expr -> Expr -> Bool
---Purpose: Given an expression E, and an expression toReplace, checks if toReplace is in E and returns True or False
---Example: hasExpr (App (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))) (App (Lambda "x" (Var "x")) (Lambda "x" (Var "x")))) (Lambda "x" (Var "x")) should return True
---Definition:
hasExpr:: Expr -> Expr -> Bool
hasExpr e toReplace = 
        case e of 
            Var n -> if (e == toReplace) then True else False
            Lambda n e1 -> if (e == toReplace) then True else (hasExpr e1 toReplace)
            App e1 e2 -> if (e == toReplace) then True else ((hasExpr e1 toReplace) || (hasExpr e2 toReplace))

---Contract: strip: Maybe Expr -> Expr
---Purpose: Remove the Just from a Maybe expression. The Nothing case should never occur. 
---Example: strip (Just (Var "x")) should produce Var "x"
---Definition:
strip::Maybe Expr -> Expr
strip e = 
    case e of 
        Just n -> n
        _ -> (Var "a") --dummy statement should never happen

---Contract: hasRedex: Expr -> Bool
---Purpose: Checks if a given expression has a redex within it, and returns a True/False value
---Example: hasRedex (Var "x") should produce False
---Definition:
hasRedex::Expr -> Bool
hasRedex e = 
    case e of 
        Var n -> False
        Lambda n e1 -> (hasRedex e1)  
        App e1 e2 -> 
            case e1 of 
                Var n1 -> (hasRedex e2) 
                App e11 e21 -> (hasRedex e11) || (hasRedex e21) || (hasRedex e1) || (hasRedex e2)
                Lambda n1 e11 -> True

---Contract: iterateForward:Expr -> [(Name, Int)] -> Expr
---Purpose: Main worker function for deBruijn
---Definition: 
iterateForward::Expr -> [(Name, Int)] -> Expr
iterateForward e l = let m = (add1toAll l) in 
    case e of 
        Var n -> if (((length m) == 0) || (getNum n m) == 0) then (Var n) else (Var (show (getNum n m)))
        Lambda n e1 -> (Lambda "_" (iterateForward e1 (addNum n [] m)))
        App e1 e2 -> (App (iterateForward e1 l) (iterateForward e2 l))

---Contract: getNum:Name -> [(Name, Int)] -> Int 
---Purpose: Helper to retrieves an integer from list m with name e
---Definition: 
getNum::Name -> [(Name, Int)] -> Int 
getNum e m = if (fst (head m) == e) then (snd (head m)) else if ((length m) == 1) then 0 else (getNum e (tail m))

---Contract: addNum:Name -> [(Name, Int)] -> [(Name, Int)] -> [(Name, Int)]
---Purpose: Helper to adds an (name,integer) pairing to list m, replacing any other values with name e
---Definition: 
addNum::Name -> [(Name, Int)] -> [(Name, Int)] -> [(Name, Int)]
addNum e m1 m2 = if ((length m2) == 0) then [(e,0)] else if (fst (head m2) == e) then (m1 ++ [(e,0)] ++ (tail m2)) else 
    if ((length m2) == 1) then (m1 ++ m2 ++ [(e,0)]) else (addNum e (m1 ++ [(head m2)]) (tail m2))

---Contract: add1toAll:[(Name, Int)] -> [(Name, Int)] 
---Purpose: Helper to add 1 to each element in the list
---Definition: 
add1toAll::[(Name, Int)] -> [(Name, Int)] 
add1toAll m = (map add1 m)

---Contract: add1:(Name, Int) -> (Name, Int)
---Purpose: Adds 1 to a (name,integer) pair
---Definition: 
add1::(Name, Int) -> (Name, Int)
add1 e = ((fst e), ((snd e) + 1))
