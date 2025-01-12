module Main where

import Test.QuickCheck (quickCheck)
import TestAutograd

main :: IO ()
main = do
    putStrLn "\nProperty based testing:"
    quickCheck prop_Add
    quickCheck prop_Mul
    quickCheck prop_Square