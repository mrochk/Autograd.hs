{-# LANGUAGE InstanceSigs #-}

module Autograd (
    Node, 
    getNodeGrad, 
    getNodeChildrenGrad,
    getNodeChildrenValues,
    scalar, namedScalar,
    add, add',
    mul, mul',
    square, square', 
    backward,
) where

data Operation = 
    Init 
    | Add 
    | Mul 
    | Square 

instance Show Operation where
    show :: Operation -> String
    show Init   = "_"
    show Add    = "+"
    show Mul    = "*"
    show Square = "^2"

-- A pair of [ child, d(node)/d(child) ].
data Child = Child Node Double deriving Show

data Node = Node {
    _name     :: String,
    _value    :: Double,
    _op       :: Operation,
    _grad     :: Double, -- gradient w.r.t root node
    _children :: [Child] -- (child, d(node)/d(child))
} deriving Show

-- Possible inputs to an operator.
data Terms = 
    One Node 
    | Two Node Node 
    | Many [Node]

{- Required functions to be implemented by each operator. -}
class Operator op where
    getName     :: op -> Terms -> String
    getValue    :: op -> Terms -> Double
    getChildren :: op -> Terms -> [Child]

instance Operator Operation where 
    getName :: Operation -> Terms -> String
    getName Add    (Two x y) = "(" ++ _name x ++ " + " ++ _name y ++ ")"
    getName Mul    (Two x y) = "(" ++ _name x ++ " * " ++ _name y ++ ")"
    getName Square (One x)   = "(" ++ _name x ++ ")^2"
    getName _      _         = error ""  

    getValue :: Operation -> Terms -> Double
    getValue Add    (Two x y) = _value x + _value y
    getValue Mul    (Two x y) = _value x * _value y
    getValue Square (One x)   = _value x ** 2
    getValue _      _         = error ""

    getChildren :: Operation -> Terms -> [Child]
    getChildren Add    (Two x y) = [Child x 1, Child y 1] -- d(x+y)/dx = 1
    getChildren Mul    (Two x y) = [Child x (_value y), Child y (_value x)] -- dxy/dx = y
    getChildren Square (One x)   = [Child x (_value x * 2)] -- dx^2/dx = 2x
    getChildren _      _         = error ""

initNode :: Double -> [Child] -> String -> Operation -> Node
initNode value children name op = Node {
    _name     = name,
    _value    = value,
    _op       = op,
    _grad     = 0,
    _children = children
}

namedScalar :: Double -> String -> Node
namedScalar value name = initNode value [] name Init

scalar :: Double -> Node
scalar value = namedScalar value (show value)

compute :: Operation -> Terms -> Node
compute op terms = initNode value children name op where 
    value    = getValue    op terms
    name     = getName     op terms 
    children = getChildren op terms

add :: Node -> Node -> Node
add x y = compute Add (Two x y)

add' :: Double -> Double -> Node
add' x y = add (scalar x) (scalar y)

mul :: Node -> Node -> Node
mul x y = compute Mul (Two x y)

mul' :: Double -> Double -> Node
mul' x y = mul (scalar x) (scalar y)

square :: Node -> Node
square x = compute Square (One x)

square' :: Double -> Node
square' = square . scalar

-- Compute the gradient of each node with respect to root node.
backward_ :: Node -> Double -> Node
backward_ node grad = Node {
    _name     = _name node,
    _value    = _value node,
    _op       = _op node,
    _grad     = _grad node + grad,
    _children = map backprop (_children node) 
} where 
    -- Propagate the gradient to a given child.
    backprop :: Child -> Child
    backprop (Child child dchild) = Child (backward_ child (grad * dchild)) dchild

backward :: Node -> Node
backward node = backward_ node 1

getNodeGrad :: Node -> Double
getNodeGrad = _grad

getNodeChildrenGrad :: Node -> [Double]
getNodeChildrenGrad node = foldr f [] (_children node) where 
    f (Child _ childGrad) grads = childGrad:grads 

getNodeChildrenValues :: Node -> [Double]
getNodeChildrenValues node = foldr f [] (_children node) where 
    f (Child child _) values = _value child:values 