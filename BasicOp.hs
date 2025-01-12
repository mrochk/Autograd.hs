{-# LANGUAGE InstanceSigs #-}

module BasicOp (
    BasicOp,
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

data BasicOp = 
    Init 
    | Add 
    | Mul 
    | Square 

instance Show BasicOp where
    show :: BasicOp -> String
    show Init   = "_"
    show Add    = "+"
    show Mul    = "*"
    show Square = "^2"

instance Operator BasicOp where 
    makeName :: BasicOp -> Terms BasicOp -> String
    makeName Add    (Two x y) = "(" ++ getName x ++ " + " ++ getName y ++ ")"
    makeName Mul    (Two x y) = "(" ++ getName x ++ " * " ++ getName y ++ ")"
    makeName Square (One x)   = "(" ++ getName x ++ ")^2"
    makeName _      _         = error ""  

    makeValue :: BasicOp -> Terms BasicOp -> Double
    makeValue Add    (Two x y) = getValue x + getValue y
    makeValue Mul    (Two x y) = getValue x * getValue y
    makeValue Square (One x)   = getValue x ** 2
    makeValue _      _         = error ""

    makeChildren :: BasicOp -> Terms BasicOp -> [Child BasicOp]
    makeChildren Add    (Two x y) = [Child x 1, Child y 1] -- d(x+y)/dx = 1
    makeChildren Mul    (Two x y) = [Child x (getValue y), Child y (getValue x)] -- dxy/dx = y
    makeChildren Square (One x)   = [Child x (getValue x * 2)] -- dx^2/dx = 2x
    makeChildren _      _         = error ""

namedScalar :: Double -> String -> (Node BasicOp)
namedScalar value name = initNode value [] name Init

scalar :: Double -> (Node BasicOp)
scalar value = namedScalar value (show value)

compute :: Operator op => op -> (Terms op) -> (Node op)
compute op terms = initNode value children name op where 
    value    = makeValue    op terms
    name     = makeName     op terms 
    children = makeChildren op terms

add :: (Node BasicOp) -> (Node BasicOp) -> (Node BasicOp)
add x y = compute Add (Two x y)

add' :: Double -> Double -> (Node BasicOp)
add' x y = add (scalar x) (scalar y)

mul :: Autograd.Node BasicOp -> Node BasicOp -> Node BasicOp
mul x y = compute Mul (Two x y)

mul' :: Double -> Double -> Node BasicOp
mul' x y = mul (scalar x) (scalar y)

square :: Node BasicOp -> Node BasicOp
square x = compute Square (One x)

square' :: Double -> Node BasicOp
square' = square . scalar

