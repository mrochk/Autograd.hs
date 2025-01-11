module Main where

import Autograd 

t = add' 10 20

main :: IO ()
main = putStrLn (show t)