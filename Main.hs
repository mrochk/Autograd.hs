module Main where

import Test.QuickCheck (quickCheck)

import TestAutograd
import Autograd
import ScalarOps
import Test.HUnit (runTestTT, showCounts, Test (TestList), putTextToShowS, runTestText, runTestTTAndExit)

main :: IO ()
main = do
    putStrLn "\nProperty based testing:"
    quickCheck prop_Identity
    quickCheck prop_Add
    quickCheck prop_Mul
    quickCheck prop_Square

    putStrLn "\nUnit testing:"
    runTestTTAndExit unitTests