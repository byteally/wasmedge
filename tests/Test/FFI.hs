{-# LANGUAGE OverloadedStrings #-}
module Test.FFI
  ( ffiTT
  ) where

import           Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import WasmEdge.Internal.FFI.Version

ffiTT :: TestTree
ffiTT = testGroup "ffi tests"
  [ versionTT
  , stringTT
  ]

versionTT :: TestTree
versionTT = testGroup "version tests"
  [
   testProperty "full version" (withTests 1 $ property $ versionGet === "0.13.3")
  , testProperty "major version" (withTests 1 $ property $ versionGetMajor === 0)
  , testProperty "minor version" (withTests 1 $ property $ versionGetMinor === 13)
  , testProperty "patch version" (withTests 1 $ property $ versionGetPatch === 3)
  ]

stringTT :: TestTree
stringTT = testGroup "string tests"
  [
  ]
  
