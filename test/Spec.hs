{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import {-@ HTF_TESTS @-} StringCalculator
import Test.Framework

main :: IO ()
main = htfMain htf_importedTests
