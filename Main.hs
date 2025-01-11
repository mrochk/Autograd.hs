module Main where

import TestAutograd (tests)
import Test.HUnit (runTestTT)

main = runTestTT tests