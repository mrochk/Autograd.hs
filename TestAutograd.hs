module TestAutograd (
    prop_Identity, prop_Add, prop_Mul, prop_Square,
    unitTests,
) where 

import Test.HUnit
import Test.QuickCheck
import qualified Data.Map as Map

import Autograd
import ScalarOps

{- Property-Based -}

prop_Identity :: Double -> Bool
prop_Identity x = (1.0) == (getGradient node) 
    where node = backward $ scalar x

prop_Add :: Double -> Double -> Bool
prop_Add x y = all (1 ==) $ getChildrenGradients node
    where node = backward $ add' x y

prop_Mul :: Double -> Double -> Bool
prop_Mul x y = getChildrenGradients node == (reverse $ getChildrenValues node)
    where node = backward $ mul' x y

prop_Square :: Double -> Bool
prop_Square x = [2 * x] == (getChildrenGradients node) 
    where node = backward $ square' x

{- Unit Testing -}

a = namedScalar 10.0 "a"
b = namedScalar 12.0 "b"
c = namedScalar 3.0 "c"

-- a + b + c
graph1 :: Autograd.Node ScalarOp 
graph1 = add (add a b) c

testGraph1 :: Test
testGraph1 = TestCase (assertEqual 
        "graph 1 - a + b + c" 
        [Just 1, Just 1, Just 1] 
        [lk "a", lk "b", lk "c"]
    ) where 
        m = gradients $ backward graph1
        lk s = Map.lookup s m

-- a * b * c
graph2 :: Autograd.Node ScalarOp
graph2 = mul (mul a b) c

testGraph2 :: Test
testGraph2 = TestCase (assertEqual 
        "graph 2 - a * b* c" 
        [Just (12*3), Just (10*3), Just (10*12)] 
        [lk "a", lk "b", lk "c"]
    ) where 
        m = gradients $ backward graph2
        lk s = Map.lookup s m

-- a + a + a
graph3 :: Autograd.Node ScalarOp 
graph3 = add (add a a) a

testGraph3 :: Test
testGraph3 = TestCase (assertEqual 
        "graph 3 - a + a + a" 
        [Just 3] 
        [lk "a"]
    ) where 
        m = gradients $ backward graph3
        lk s = Map.lookup s m

-- a * a * a
graph4 :: Autograd.Node ScalarOp 
graph4 = mul (mul a a) a

testGraph4 :: Test
testGraph4 = TestCase (assertEqual 
        "graph 4 - a * a * a" 
        [Just (3*10*10)] 
        [lk "a"]
    ) where 
        m = gradients $ backward graph4
        lk s = Map.lookup s m

--------------------------------------------------------------------------------

unitTests :: Test
unitTests = TestList [
        testGraph1, 
        testGraph2,
        testGraph3,
        testGraph4
    ]