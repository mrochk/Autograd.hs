{-# LANGUAGE InstanceSigs #-}

module ScalarOps (
    ScalarOp,
    scalar, 
    namedScalar,
    add, 
    add',
    mul, 
    mul',
    square, 
    square', 
) where

import Autograd

data ScalarOp = 
    Init   |
    Add    |
    Mul    |
    Square 

instance Show ScalarOp where
    show :: ScalarOp -> String
    show Init   = "_"
    show Add    = "+"
    show Mul    = "*"
    show Square = "^2"

instance Operator ScalarOp where 
    makeName :: ScalarOp -> Terms ScalarOp -> String
    makeName Add    (Two x y) = "(" ++ getName x ++ " + " ++ getName y ++ ")"
    makeName Mul    (Two x y) = "(" ++ getName x ++ " * " ++ getName y ++ ")"
    makeName Square (One x)   = "(" ++ getName x ++ ")^2"
    makeName _      _         = error ""  

    makeValue :: ScalarOp -> Terms ScalarOp -> Double
    makeValue Add    (Two x y) = getValue x + getValue y
    makeValue Mul    (Two x y) = getValue x * getValue y
    makeValue Square (One x)   = getValue x ** 2
    makeValue _      _         = error ""

    makeChildren :: ScalarOp -> Terms ScalarOp -> [Child ScalarOp]
    makeChildren Add    (Two x y) = [(x, 1), (y, 1)] -- d(x+y)/dx = 1
    makeChildren Mul    (Two x y) = [(x, getValue y), (y, getValue x)] -- dxy/dx = y
    makeChildren Square (One x)   = [(x, getValue x*2)] -- dx^2/dx = 2x
    makeChildren _      _         = error ""

namedScalar :: Double -> String -> (Node ScalarOp)
namedScalar value name = makeNode value [] name Init

scalar :: Double -> (Node ScalarOp)
scalar value = namedScalar value ("_u" ++ show value)

add :: (Node ScalarOp) -> (Node ScalarOp) -> (Node ScalarOp)
add x y = compute Add (Two x y)

add' :: Double -> Double -> (Node ScalarOp)
add' x y = add (scalar x) (scalar y)

mul :: Node ScalarOp -> Node ScalarOp -> Node ScalarOp
mul x y = compute Mul (Two x y)

mul' :: Double -> Double -> Node ScalarOp
mul' x y = mul (scalar x) (scalar y)

square :: Node ScalarOp -> Node ScalarOp
square x = compute Square (One x)

square' :: Double -> Node ScalarOp
square' x = square (scalar x) 

