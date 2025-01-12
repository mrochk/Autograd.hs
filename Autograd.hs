{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Autograd (
    Operator(..),
    Node(..), 
    Terms(..), 
    Child(..), 
    backward,
    getNodeChildrenGrad, 
    getNodeChildrenValues,
) where

{- Required functions to be implemented by each operator. -}
class (Show op) => Operator op where
    makeName     :: op -> (Terms op) -> String
    makeValue    :: op -> (Terms op) -> Double
    makeChildren :: op -> (Terms op) -> [Child op]

-- Possible inputs to an operator.
data Terms op = Operator op =>
    One (Node op)
    | Two (Node op) (Node op) 
    | Many [(Node op)]

-- A pair of [ child, d(node)/d(child) ].
data Child op = Operator op => Child (Node op) Double 

deriving instance Show (Child op)

data Node op = Operator op => Node {
    _name     :: String,
    _value    :: Double,
    _op       :: op,
    _grad     :: Double, -- gradient w.r.t root node
    _children :: [Child op] -- (child, d(node)/d(child))
} 

deriving instance Show (Node op)

-- Compute the gradient of each node with respect to root node.
backward_ :: Operator op => Node op -> Double -> Node op
backward_ node grad = Node {
    _name     = _name node,
    _value    = _value node,
    _op       = _op node,
    _grad     = _grad node + grad,
    _children = map backprop (_children node) 
} where 
    -- Propagate the gradient to a given child.
    backprop :: Child op -> Child op
    backprop (Child child dchild) = Child (backward_ child (grad * dchild)) dchild

backward :: Operator op => Node op -> Node op
backward node = backward_ node 1

getNodeGrad :: Node op -> Double
getNodeGrad = _grad

getNodeChildrenGrad :: Node op -> [Double]
getNodeChildrenGrad node = foldr f [] (_children node) where 
    f (Child _ childGrad) grads = childGrad:grads 

getNodeChildrenValues :: Node op -> [Double]
getNodeChildrenValues node = foldr f [] (_children node) where 
    f (Child child _) values = _value child:values 