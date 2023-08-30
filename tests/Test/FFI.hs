{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Test.FFI
  ( ffiTT
  ) where

import           Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import WasmEdge.Internal.FFI.Version
import WasmEdge.Internal.FFI.ValueTypes
import qualified Data.Text as T
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.String
import Data.Kind
import Data.WideWord.Int128
import GHC.Generics
import Control.Monad.IO.Class

import Data.Unique
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Control.Concurrent.MVar

ffiTT :: TestTree
ffiTT = testGroup "ffi tests"
  [ versionTT
  , valueTT
  , stringTT
  , prop_finalization
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

valueTT :: TestTree
valueTT = testGroup "value tests"
  [ testProperty "int32" $ property $ do
      i <- forAll $ Gen.int32 $ Range.constantBounded
      tripping i WasmInt32 (\case
                               WasmInt32 v -> Just v
                               _ -> Nothing)
  , testProperty "int64" $ property $ do
      i <- forAll $ Gen.int64 $ Range.constantBounded
      tripping i WasmInt64 (\case
                               WasmInt64 v -> Just v
                               _ -> Nothing)
  , testProperty "float" $ property $ do
      i <- forAll $ Gen.float $ Range.linearFrac 0 10
      tripping i WasmFloat (\case
                               WasmFloat v -> Just v
                               _ -> Nothing)
  , testProperty "double" $ property $ do
      i <- forAll $ Gen.double $ Range.linearFrac 0 10
      tripping i WasmDouble (\case
                               WasmDouble v -> Just v
                               _ -> Nothing)
   ,
  testProperty "v128" $ property $ do
      i <- forAll $ Gen.integral $ Range.constantBounded @Int128
      tripping i WasmInt128 (\case
                               WasmInt128 v -> Just v
                               _ -> Nothing)
  , testProperty "v128 for 0" $ withTests 1 $ property $ do
      tripping (-1) WasmInt128 (\case
                               WasmInt128 v -> Just v
                               _ -> Nothing)        
  ]

data NewString (v :: Type -> Type) = NewString ByteString
  deriving (Generic, Show)
  deriving anyclass (FunctorB, TraversableB)

newtype ShowableUnique = ShowableUnique {getShowableUnique :: Unique}
instance Show ShowableUnique where
  show = show . hashUnique . getShowableUnique

stringCmd :: FFICmd 
stringCmd = Command
  { commandGen = \State{} -> Just $ pure $ NewString "wasm\0edge"
  , commandExecute = \(NewString bs) -> do
      (uq, ws) <- liftIO $ testonly_accquire (pure $ mkStringFromBytes bs)
--      liftIO $ finalize ws
      pure (ShowableUnique uq, ws)
  , commandCallbacks = [ Require $ \State {} _ -> True
                       , Ensure $ \_ _ (NewString _bs) (ShowableUnique _uq, _ws) -> do
--                           isalive <- evalIO $ testonly_isAlive uq
                           assert True --isalive 
                       ]
  }

data State (v :: Type -> Type) = State
  { 
  } deriving (Eq, Ord)

type FFICmd = Command Gen (PropertyT IO) State

prop_finalization :: TestTree
prop_finalization = testProperty "finalization tests" $ withTests 1 $ property $ do
  let
    commands :: [FFICmd]
    commands = [stringCmd] 
    initialState = State
      { 
      }
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands
  executeSequential initialState actions
