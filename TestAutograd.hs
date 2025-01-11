module TestAutograd (tests) where 

import Test.HUnit
import Test.QuickCheck
import Autograd
import BasicOp

makeTestAdd :: Double -> Double -> Test
makeTestAdd x y = TestCase (assertBool "Add: d(x+y)/d[x, y] == [1, 1]" 
        (all (1 ==) (getNodeChildrenGrad n))
    ) where n = backward (add' x y)

makeTestMul :: Double -> Double -> Test
makeTestMul x y = TestCase (assertEqual "Mul: d(xy)/d[x, y] == [y, x]" 
        (getNodeChildrenGrad n) 
        ((reverse . getNodeChildrenValues) n)
    ) where n = backward (mul' x y)

makeTestSquare :: Double -> Test
makeTestSquare x = TestCase (assertEqual "Square: d(x^2)/dx == 2x" 
        [2 * x] (getNodeChildrenGrad n)
    ) where n = backward (square' x)

tests :: Test
tests = TestList [
        -- (*)
        TestLabel "0 * 0"         (makeTestMul 0     0    ),
        TestLabel "1 * 2"         (makeTestMul 1     2    ),
        TestLabel "12345 * 23456" (makeTestMul 12345 23456),
        TestLabel "-1 * -2"       (makeTestMul (-1)  (-2) ),
        TestLabel "-1 * 2"        (makeTestMul (-1)  2    ),
        -- (+)
        TestLabel "0 + 0" (makeTestAdd 0 0),
        TestLabel "1 + 1" (makeTestAdd 1 1),
        -- (^2)
        TestLabel "0^2"     (makeTestSquare 0    ),
        TestLabel "10^2"    (makeTestSquare 10   ),
        TestLabel "(-10)^2" (makeTestSquare (-10))
    ]