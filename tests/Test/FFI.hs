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
import WasmEdge.Internal.FFI.ValueTypes
import qualified Data.Text as T
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.String
--import Control.Monad.IO.Class

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
  [ testProperty "null-terminated string" $ withTests 1 $ property $ (tripping "wasm\0edge" (mkStringFromBytes . Char8.pack) (Just . T.unpack . toText))
  , testProperty "null-terminated fromString" $ withTests 1 $ property $ (tripping "wasm\0edge" (fromString @WasmString) (Just . T.unpack . toText))
  , testProperty "finalizeString" $ withTests 1 $ property $ do
      let ws = "foo" :: WasmString
      toText ws === "foo"
  ]
  
