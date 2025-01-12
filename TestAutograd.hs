module TestAutograd (
    prop_Add,
    prop_Mul,
    prop_Square,
) where 

import Test.QuickCheck
import Autograd
import BasicOp

prop_Add :: Double -> Double -> Bool
prop_Add x y = all (1 ==) $ getNodeChildrenGrad node
    where node = backward (add' x y)

prop_Mul :: Double -> Double -> Bool
prop_Mul x y = getNodeChildrenGrad node == (reverse $ getNodeChildrenValues node)
    where node = backward (mul' x y)

prop_Square :: Double -> Bool
prop_Square x = [2 * x] == (getNodeChildrenGrad node) 
    where node = backward (square' x)