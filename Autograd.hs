{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Autograd (
    Operator(..),
    Node(Node), makeNode,
    Terms(..), 
    Child(..), 
    gradients,
    backward,
    getName,
    getValue,
    getNodeChildrenGrad, 
    getNodeChildrenValues,
) where

import qualified Data.Map  as Map 

{- Required functions to be implemented by each operator. -}
class (Show op) => Operator op where
    {- makeName specifies how to combine the terms' names. -}
    makeName :: op -> (Terms op) -> String
    {- makeValue specifies how to combine the terms' values. -}
    makeValue :: op -> (Terms op) -> Double
    {- makeValue specifies the gradient of each child wrt node. -}
    makeChildren :: op -> (Terms op) -> [Child op]

{- Possible inputs to an operator. -}
data Terms op = Operator op =>
    One (Node op) | Two (Node op) (Node op) | Many [(Node op)]

{- A pair of [ child, d(node)/d(child) ]. -}
type Child op = (Node op , Double)

{- A node of the computation graph. -}
data Node op = Operator op => Node {
    _name     :: String,
    _value    :: Double,
    _op       :: op,
    _grad     :: Double,    -- gradient w.r.t root node
    _children :: [Child op] -- (child, d(node)/d(child))
}

deriving instance Show (Node op)

{- Instantiate a new node. -}
makeNode :: Operator op => Double -> [Child op] -> String -> op -> (Node op)
makeNode value children name op = Node {
    _name     = name,
    _value    = value,
    _op       = op,
    _grad     = 0,
    _children = children
}

{- Compute the gradient of each node with respect to root node. -}
backward_ :: Operator op => Node op -> Double -> Node op
backward_ node grad = Node {
    _name     = _name  node,
    _value    = _value node,
    _op       = _op    node,
    _grad     = _grad  node + grad,
    _children = map backprop (_children node) 
} where 
    {- Propagate the gradient to a given child. -}
    backprop :: Operator op => Child op -> Child op
    backprop (child, dchild) = (,) (backward_ child (grad * dchild)) dchild

{- Compute the gradient of each node with respect to root node. -}
backward :: Operator op => Node op -> Node op
backward node = backward_ node 1

{- Get all gradients in map, adding gradients if necessary. -}
gradients :: Node op -> Map.Map String Double  
gradients root = fillMap root Map.empty

{- Get all gradients in map, adding gradients if necessary. -}
fillMap :: Node op -> Map.Map String Double -> Map.Map String Double
fillMap node m 
  | null children = adjustedMap
  | otherwise     = foldr union Map.empty children
  where 
    children             = getChildren node
    gradient             = getNodeGrad node
    identifier           = getName     node
    adjustedMap          = insertOrAdd identifier gradient m  
    union (child, _) map = Map.unionWith (+) (fillMap child adjustedMap) map

{- Insert value in map if not present, otherwise add it to value already there. -}
insertOrAdd :: (Ord a, Num b) => a -> b -> Map.Map a b -> Map.Map a b
insertOrAdd key value map = case Map.lookup key map of 
      Nothing -> Map.insert key value map
      Just  _ -> Map.adjust (+ value) key map

getNodeGrad :: Node op -> Double
getNodeGrad = _grad

getName :: Node op -> String
getName = _name

getValue :: Node op -> Double
getValue = _value

getChildren :: Node op -> [Child op]
getChildren = _children

getNodeChildrenGrad :: Node op -> [Double]
getNodeChildrenGrad node = foldr f [] (_children node) where 
    f (_, childGrad) grads = childGrad:grads 

getNodeChildrenValues :: Node op -> [Double]
getNodeChildrenValues node = foldr f [] (_children node) where 
    f (child, _) values = getValue child:values 