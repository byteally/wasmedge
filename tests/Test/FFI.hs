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
import WasmEdge.Internal.FFI.Bindings
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.String
import Data.Kind
import GHC.Generics
import Control.Monad
import Control.Monad.IO.Class
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V

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
  , testProperty "C String" $ withTests 1 $ property $ (tripping "Testing string" (unsafePerformIO.stringCreateByCString) (Just. T.unpack. toText))
  , testProperty "finalizeString" $ withTests 1 $ property $ do
      let ws = "foo" :: WasmString
      toText ws === "foo"
  , testProperty "Check length of wasmString" $ withTests 1 $ property $ do
      let ws = "foo" :: WasmString
      (wasmStringLength ws) === 3
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
  , testProperty "v128" $ property $ do
      i <- forAll $ Gen.integral $ Range.constantBounded @Int128
      tripping i WasmInt128 (\case
                               WasmInt128 v -> Just v
                               _ -> Nothing)
  , testProperty "Haskell Ref" $ withTests 1 $ property $ do
      let testStr =  ("hello from Haskell" :: String)
      tripping testStr (WasmExternRef . unsafePerformIO . toHsRef) (\case
                               WasmExternRef v -> unsafePerformIO $ fromHsRef @String v
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
  liftIO $ test1
  liftIO $ test2
  liftIO $ test3
  liftIO $ test4
  liftIO $ test5
  liftIO $ test6
  liftIO $ test7
  liftIO $ test8
  liftIO $ test9
  liftIO $ testAsyncRun
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands
  executeSequential initialState actions

test1 :: IO ()
test1 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    _ <- withWasmResT (vMCreate cfgCxt Nothing) $ \vm -> do
      addTwoRes <- vMRunWasmFromFile vm "./tests/sample/wasm/addTwo.wasm" "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3]) 1
      print addTwoRes
      pure ()
    pure ()

test2 :: IO ()
test2 = do
  wasmBS <- BS.readFile "./tests/sample/wasm/addTwo.wasm"
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    _ <- withWasmResT (vMCreate cfgCxt Nothing) $ \vm -> do
      addTwoRes <- vMRunWasmFromBuffer vm wasmBS "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3]) 1
      print addTwoRes
      pure ()
    pure ()

test3 :: IO ()
test3 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do    
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
      (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
      let astMod = maybe (error "Failed to load AST module") id astModMay
      _ <- withWasmResT (vMCreate cfgCxt Nothing) $ \vm -> do
        addTwoRes <- vMRunWasmFromASTModule vm astMod "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3]) 1
        print addTwoRes
      pure ()
    pure ()

test4 :: IO ()
test4 = do
  wasmBS <- BS.readFile "./tests/sample/wasm/addTwo.wasm"
  void $ withWasmResT configureCreate $ \cfgCxt -> do    
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
      (_, astModMay) <- loaderParseFromBuffer loader wasmBS
      let astMod = maybe (error "Failed to load AST module") id astModMay
      _ <- withWasmResT (vMCreate cfgCxt Nothing) $ \vm -> do
        addTwoRes <- vMRunWasmFromASTModule vm astMod "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3]) 1
        print addTwoRes
      pure ()
    pure ()

-- Validator
test5 :: IO ()
test5 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do    
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
      (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
      let astMod = maybe (error "Failed to load AST module") id astModMay
      void $ withWasmResT (validatorCreate cfgCxt) $ \validator -> do
        vres <- validatorValidate validator astMod
        print vres
      pure ()

-- executor
test6 :: IO ()
test6 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (executorCreate (Just cfgCxt) Nothing) $ \exec -> do
      void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
        (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
        let astMod = maybe (error "Failed to load AST module") id astModMay
        void $ withWasmResT (validatorCreate cfgCxt) $ \validator -> do
          vres <- validatorValidate validator astMod
          print vres
          void $ withWasmResT (storeCreate) $ \store -> do
            (eres, _modInst) <- executorRegister exec store astMod "mod"
            print ("exec"::String, eres)

-- Register an existing Module instance and export the module name
test7 :: IO ()
test7 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (executorCreate (Just cfgCxt) Nothing) $ \exec -> do
      void $ withWasmResT (storeCreate) $ \store -> do
        void $ withWasmResT (moduleInstanceCreate "host-module") $ \modInst -> do
          eres <- executorRegisterImport exec store modInst
          print ("execImp"::String, eres)

-- Instantiate an AST module to an anonymous Module instance
test8 :: IO ()
test8 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (executorCreate (Just cfgCxt) Nothing) $ \exec -> do
      void $ withWasmResT (storeCreate) $ \store -> do
        void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
          (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
          let astMod = maybe (error "Failed to load AST module") id astModMay
          void $ withWasmResT (validatorCreate cfgCxt) $ \validator -> do
            _vres <- validatorValidate validator astMod
            -- Not validating before Instantiate throws an error
            (eres, _modInst) <- executorInstantiate exec store astMod
            print ("execInst"::String, eres)

-- Invoke functions
test9 :: IO ()
test9 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (executorCreate (Just cfgCxt) Nothing) $ \exec -> do
      void $ withWasmResT (storeCreate) $ \store -> do
        void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
          (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
          let astMod = maybe (error "Failed to load AST module") id astModMay
          void $ withWasmResT (validatorCreate cfgCxt) $ \validator -> do
            _vres <- validatorValidate validator astMod
            -- Not validating before Instantiate throws an error
            (_eres, _modInst) <- executorInstantiate exec store astMod
            Just _fnInst <- moduleInstanceFindFunction _modInst "addTwo"
            res <- executorInvoke exec _fnInst (V.fromList [WasmInt32 1, WasmInt32 3])
            print res

testAsyncRun :: IO ()
testAsyncRun = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    _ <- withWasmResT (vMCreate cfgCxt Nothing) $ \_vm -> do
      _addTwoAsync <- vMAsyncRunWasmFromFile _vm "./tests/sample/wasm/addTwo.wasm" "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3])
      -- TODO: Fix: Not waiting sometimes segv. Chcck the lifetime of WasmVal passed or async returned(most likely). If later, try cancel before finalization
      retLen <- asyncGetReturnsLength _addTwoAsync
      res <- asyncGet _addTwoAsync retLen
      print ("AsyncRet" :: String, res)
      pure ()
    pure ()
  
          
        
      
  
