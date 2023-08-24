module Main (main) where

import Test.FFI
import Test.Tasty

main :: IO ()
main = do
  defaultMain ffiTT

