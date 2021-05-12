# Lambda-Calculus-Reducer

A basic lambda calculus reducer that supports applicative and normal order reductions, along with conversion to deBruijn index form. 

## Description
Expressions take the form shown in the Data.hs file, so applications and lambda are explicitly typed out. For example, \x.xx would take the form (Lambda "x" (App (Var "x") (Var "x"))).  

## Usage
Run ghci in the directory of the the files. Then load the Interpreter.hs file with ":l Interpreter".

## Commands
normNF_OneStep will perform one normal order reduction on the given expression.

appNF_OneStep will perform one applicative order reduction on the given expression.

normNF takes in two arguments, n and an expression, and will perform n normal order reductions on the given expression.

appNF takes in two arguments, n and an expression, and will perform n applicative order reductions on the given expression. 

reduceToNorm will reduce an expression to normal form, unless the form cannot be reduced any further due to infinite repeating, in which case it will return that expression. 

deBruijn will convert an expression to beBruijn index form. 
