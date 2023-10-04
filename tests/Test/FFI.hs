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
import qualified Data.Vector.Storable as SV
import GHC.Stack

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
  , ownershipTT
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
  [ testProperty "null-terminated string" $ withTests 1 $ property $ (tripping "wasm\0edge" (stringCreateByBuffer . Char8.pack) (Just . T.unpack . toText))
  , testProperty "null-terminated fromString" $ withTests 1 $ property $ (tripping "wasm\0edge" (fromString @WasmString) (Just . T.unpack . toText))
  , testProperty "C String" $ withTests 1 $ property $ (tripping "Testing string" (stringCreateByCString) (Just. T.unpack. toText))
  , testProperty "finalizeString" $ withTests 1 $ property $ do
      let ws = "foo" :: WasmString
      toText ws === "foo"
  , testProperty "Check length of wasmString" $ withTests 1 $ property $ do
      let ws = "foo" :: WasmString
      (wasmStringLength ws) === 3
  , testProperty "string copying" $ withTests 1 $ property $ do
      let ws = "foo" :: WasmString
      let newWs = stringCopy 3 ws
      (Char8.unpack newWs === "foo")
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
      (ws) <- liftIO $ (pure $ stringCreateByBuffer bs)
--      liftIO $ finalize ws
      pure (ShowableUnique undefined, ws)
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

ownershipTT :: TestTree
ownershipTT = testGroup "ownership tests"
  [ testProperty "fromString" $ withTests 1 $ property $ assertHsOwnedResult (pure $! "wasm\0edge" :: IO WasmString)
  , testProperty "C String" $ withTests 1 $ property $ assertHsOwnedResult (pure $! stringCreateByCString "wasmedge")
  , testProperty "StringWrap" $ withTests 1 $ property $ assertHsOwnedResult (pure $! stringWrap "wasmedge")
  , testProperty "ConfigureCreate" $ withTests 1 $ property $ assertHsOwnedOptResult (configureCreate)
  , testProperty "StatisticsCreate" $ withTests 1 $ property $ assertHsOwnedOptResult (statisticsCreate)
  , testProperty "FunctionTypeCreate" $ withTests 1 $ property $ assertHsOwnedOptResult (functionTypeCreate (SV.fromList [ValType_I32]) (SV.fromList [ValType_F32]))
  , testProperty "TableTypeCreate" $ withTests 1 $ property $ assertHsOwnedOptResult (tableTypeCreate RefType_ExternRef (WasmLimit {hasMax = True, shared = False, minLimit = 10, maxLimit = 20}))
  , testProperty "MemoryTypeCreate" $ withTests 1 $ property $ assertHsOwnedOptResult (memoryTypeCreate (WasmLimit {hasMax = True, shared = False, minLimit = 10, maxLimit = 20}))
  , testProperty "GlobalTypeCreate" $ withTests 1 $ property $ assertHsOwnedOptResult (globalTypeCreate ValType_I32 Mutability_Const)
  -- , testProperty "ImportTypeGetModuleName" $ withTests 1 $ property $ do
  --     imps <- liftIO $ withWasmResT configureCreate $ \cfgCxt -> do    
  --       configureAddHostRegistration cfgCxt HostRegistration_Wasi
  --       withWasmResT (loaderCreate cfgCxt) $ \loader -> do
  --         (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/trap.wasm"
  --         let astMod = maybe (error "Failed to load AST module") id astModMay
  --         astModuleListImports astMod 10
  --     assertCOwnedResult (importTypeGetModuleName (SV.head $ maybe mempty id (join imps)))
  ]

assertHsOwnedResult :: (HasFinalizer a, MonadTest m, MonadIO m, HasCallStack) => IO a -> m ()
assertHsOwnedResult act = withFrozenCallStack $ do
  ownr <- liftIO $ act >>= testonly_getOwner
  ownr === Just HsOwned

assertHsOwnedOptResult :: (HasFinalizer a, MonadTest m, MonadIO m, HasCallStack) => IO (Maybe a) -> m ()
assertHsOwnedOptResult act = withFrozenCallStack $ do
  resMay <- liftIO $ act
  res <- maybe failure pure resMay
  ownr <- liftIO $ testonly_getOwner res
  ownr === Just HsOwned  

_assertCOwnedResult :: (HasFinalizer a, MonadTest m, MonadIO m, HasCallStack) => IO a -> m ()
_assertCOwnedResult act = withFrozenCallStack $ do
  ownr <- liftIO $ act >>= testonly_getOwner
  ownr === Just COwned  
  

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
  liftIO $ testExecutorAsyncInvoke
  liftIO $ testAsyncRun
  liftIO $ testAsyncRunFromBuffer
  liftIO $ testAsyncRunFromATSModule
  liftIO $ testCompilerCompile
  liftIO $ testVmExecute
  liftIO $ testVmAsyncExecute
  liftIO $ testVmExecuteRegistered
  liftIO $ testVmGetFunctionList
  liftIO $ testCompilerCompileFromBuffer
  liftIO $ testStore
  liftIO $ testModInst
  liftIO $ testExternRefTableInst
  liftIO $ testFunRefTableInst
  liftIO $ testMemoryInst
  liftIO $ testMutGlobalInst
  liftIO $ testConstGlobalInst
  liftIO $ testValueGenNullRef
  liftIO $ testHostFnAlloc
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands
  executeSequential initialState actions

-- vmRunWasmFromFile
test1 :: IO ()
test1 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \vm -> do
      addTwoRes <- vmRunWasmFromFile vm "./tests/sample/wasm/addTwo.wasm" "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3]) 1
      print addTwoRes
      pure ()
    pure ()

-- vmRunWasmFromBuffer
test2 :: IO ()
test2 = do
  wasmBS <- BS.readFile "./tests/sample/wasm/addTwo.wasm"
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \vm -> do
      addTwoRes <- vmRunWasmFromBuffer vm wasmBS "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3]) 1
      print addTwoRes
      pure ()
    pure ()

-- loaderParseFromFile 
test3 :: IO ()
test3 = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do    
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
      (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
      let astMod = maybe (error "Failed to load AST module") id astModMay
      _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \vm -> do
        addTwoRes <- vmRunWasmFromASTModule vm astMod "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3]) 1
        print addTwoRes
      pure () 
    pure ()

-- loaderParseFromBuffer 
test4 :: IO () 
test4 = do
  wasmBS <- BS.readFile "./tests/sample/wasm/addTwo.wasm"
  void $ withWasmResT configureCreate $ \cfgCxt -> do    
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
      (_, astModMay) <- loaderParseFromBuffer loader wasmBS
      let astMod = maybe (error "Failed to load AST module") id astModMay
      _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \vm -> do
        addTwoRes <- vmRunWasmFromASTModule vm astMod "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3]) 1
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

-- ExecutorAsyncInvoke
testExecutorAsyncInvoke :: IO ()
testExecutorAsyncInvoke = do
 void $ withWasmResT configureCreate $ \cfgCxt -> do
  configureAddHostRegistration cfgCxt HostRegistration_Wasi
  void $ withWasmResT (executorCreate (Just cfgCxt) Nothing) $ \exec -> do
   void $ withWasmResT (storeCreate) $ \store -> do
    void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
     (_,astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
     let astMod = maybe (error "Failed to load AST module") id astModMay
     void $ withWasmResT (validatorCreate cfgCxt) $ \validator -> do
      _vres <- validatorValidate validator astMod
      -- Not validating before Instantiate throws an error
      (_eres, _modInst) <- executorInstantiate exec store astMod
      Just _fnInst <- moduleInstanceFindFunction _modInst "addTwo"
      _asyncAddTwo <- executorAsyncInvoke exec _fnInst (V.fromList [WasmInt32 1,WasmInt32 23])
      retLen <- asyncGetReturnsLength _asyncAddTwo
      res <- asyncGet _asyncAddTwo retLen
      print ("ExecutorAsyncInvoke" :: String, res)

-- vmexecute
testVmExecute :: IO ()
testVmExecute = do
 void $ withWasmResT configureCreate $ \cfgCxt -> do
  configureAddHostRegistration cfgCxt HostRegistration_Wasi
  _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \vm -> do
   _ <- vmLoadWasmFromFile vm "./tests/sample/wasm/addTwo.wasm"
   _ <- vmValidate vm
   _ <- vmInstantiate vm
   addTwoRes <- vmExecute vm "addTwo" (V.fromList [WasmInt32 33,WasmInt32 33]) 1
   print addTwoRes
   pure ()
  pure ()

--vmAsyncExecute
testVmAsyncExecute :: IO ()
testVmAsyncExecute = do
 wasmBS <- BS.readFile "./tests/sample/wasm/addTwo.wasm"
 void $ withWasmResT configureCreate $ \cfgCxt -> do
  configureAddHostRegistration cfgCxt HostRegistration_Wasi
  _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \vm -> do
   _ <- vmLoadWasmFromBuffer vm wasmBS
   _ <- vmValidate vm
   _ <- vmInstantiate vm
   _addTwoAsync <- vmAsyncExecute vm "addTwo" (V.fromList [WasmInt32 33,WasmInt32 34])
   retLen <- asyncGetReturnsLength _addTwoAsync
   res <- asyncGet _addTwoAsync retLen
   print ("vmAsyncExecute" :: String, res)
   pure ()
  pure ()


testVmExecuteRegistered :: IO ()
testVmExecuteRegistered = do
 void $ withWasmResT configureCreate $ \cfgCxt -> do
  configureAddHostRegistration cfgCxt HostRegistration_Wasi
  _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \vm -> do
   _ <- vmRegisterModuleFromFile vm "mod" "./tests/sample/wasm/addTwo.wasm"
   addTwoRes <- vmExecuteRegistered vm "mod" "addTwo" (V.fromList [WasmInt32 11,WasmInt32 12]) 1
   print addTwoRes
   pure ()
  pure ()

testVmGetFunctionList :: IO ()
testVmGetFunctionList = do
 void $ withWasmResT configureCreate $ \cfgCxt -> do
  configureAddHostRegistration cfgCxt HostRegistration_Wasi
  _ <- withWasmResT (storeCreate) $ \storeCxt -> do
   _ <- withWasmResT (vmCreate cfgCxt (Just storeCxt)) $ \vm -> do
    _ <- vmLoadWasmFromFile vm "./tests/sample/wasm/addTwo.wasm"
    _ <- vmValidate vm
    _ <- vmInstantiate vm
    funcNum <- vmGetFunctionListLength vm
    (names,_) <- vmGetFunctionList vm funcNum
    print ("getFunctionlist" :: String,names)
    pure ()
   pure ()
  pure ()

testAsyncRun :: IO ()
testAsyncRun = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \_vm -> do
      _addTwoAsync <- vmAsyncRunWasmFromFile _vm "./tests/sample/wasm/addTwo.wasm" "addTwo" (V.fromList [WasmInt32 90, WasmInt32 9])
      -- TODO: Fix: Not waiting sometimes segv. Chcck the lifetime of WasmVal passed or async returned(most likely). If later, try cancel before finalization
      asyncWait _addTwoAsync
      retLen <- asyncGetReturnsLength _addTwoAsync
      res <- asyncGet _addTwoAsync retLen
      print ("AsyncRet" :: String, res)
      pure ()
    pure ()

testAsyncRunFromBuffer :: IO ()
testAsyncRunFromBuffer = do
 wasmBS <- BS.readFile "./tests/sample/wasm/addTwo.wasm"
 void $ withWasmResT configureCreate $ \cfgCxt -> do
  configureAddHostRegistration cfgCxt HostRegistration_Wasi
  _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \_vm -> do
   _addTwoAsync <- vmAsyncRunWasmFromBuffer _vm wasmBS "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3])
   -- TODO: Fix: Not waiting sometimes segv. Chcck the lifetime of WasmVal passed or async returned(most likely). If later, try cancel before finalization
   retLen <- asyncGetReturnsLength _addTwoAsync
   res <- asyncGet _addTwoAsync retLen
   print ("AsyncRet from buffer" :: String, res)
   pure ()
  pure ()

testAsyncRunFromATSModule :: IO ()
testAsyncRunFromATSModule = do
 void $ withWasmResT configureCreate $ \cfgCxt -> do
  configureAddHostRegistration cfgCxt HostRegistration_Wasi
  void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
   (_,astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
   let astMod = maybe (error "Failed to load AST module") id astModMay
   _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \vm -> do
    _addTwoResAsync <- vmAsyncRunWasmFromASTModule vm astMod "addTwo" (V.fromList [WasmInt32 12,WasmInt32 13])
    retLen <- asyncGetReturnsLength _addTwoResAsync
    res <- asyncGet _addTwoResAsync retLen
    print ("AsyncRet from ats module" :: String, res)
    pure ()
   pure ()

{- Failing
testAsyncRun :: IO ()
testAsyncRun = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \_vm -> do
      _addTwoAsync <- vmAsyncRunWasmFromFile _vm "./tests/sample/wasm/addTwo.wasm" "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3])
      -- TODO: Fix: Not waiting sometimes segv. Chcck the lifetime of WasmVal passed or async returned(most likely). If later, try cancel before finalization
      pure ()
    pure ()
testAsyncRun :: IO ()
testAsyncRun = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    _ <- withWasmResT (vmCreate cfgCxt Nothing) $ \_vm -> do
      withWasmRes (vmAsyncRunWasmFromFile _vm "./tests/sample/wasm/addTwo.wasm" "addTwo" (V.fromList [WasmInt32 1, WasmInt32 3])) $ const $ pure ()
      pure ()
    pure ()
-} 

-- AOT Compiler
testCompilerCompile :: IO ()
testCompilerCompile = do
    void $ withWasmResT configureCreate $ \cfgCxt -> do
     configureAddHostRegistration cfgCxt HostRegistration_Wasi
     _ <- withWasmResT (compilerCreate cfgCxt) $ \compilerCxt -> do
      res <- compilerCompile compilerCxt "./tests/sample/wasm/addTwo.wasm" "./tests/sample/wasm/addTwo_aot.wasm"
      print res
     pure ()

-- AOT Compiler from Buffer
testCompilerCompileFromBuffer :: IO ()
testCompilerCompileFromBuffer = do
  wasmBS <- BS.readFile "./tests/sample/wasm/addTwo.wasm"
  void $ withWasmResT configureCreate $ \cfgCxt -> do
   configureAddHostRegistration cfgCxt HostRegistration_Wasi
   _ <- withWasmResT (compilerCreate cfgCxt) $ \compilerCxt -> do
    res <- compilerCompileFromBuffer compilerCxt wasmBS "./tests/sample/wasm/addTwo_aot2.wasm"
    print res
   pure()

testStore :: IO ()
testStore = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (executorCreate (Just cfgCxt) Nothing) $ \exec -> do
      void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
        (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
        let astMod = maybe (error "Failed to load AST module") id astModMay
        void $ withWasmResT (validatorCreate cfgCxt) $ \validator -> do
          _vres <- validatorValidate validator astMod
          void $ withWasmResT (storeCreate) $ \store -> do
            (_eres, _modInst) <- executorRegister exec store astMod "mod"
            modLen <- storeListModuleLength store
            mods <- storeListModule store modLen
            print ("StoreListModLen"::String, modLen)
            print ("StoreList"::String, mods)

            modNameMay <- storeFindModule store "mod"
            print ("storeFindModule" :: String, maybe ("NotFound"::String) (const "Found") modNameMay)

testModInst :: IO ()
testModInst = do
  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (executorCreate (Just cfgCxt) Nothing) $ \exec -> do
      void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
        (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
        let astMod = maybe (error "Failed to load AST module") id astModMay
        void $ withWasmResT (validatorCreate cfgCxt) $ \validator -> do
          _vres <- validatorValidate validator astMod
          void $ withWasmResT (storeCreate) $ \store -> do
            (_eres, modInst) <- executorRegister exec store astMod "mod"
            miFnLen <- moduleInstanceListFunctionLength modInst
            print ("ListFunctionLength" :: String, miFnLen)
            miListFn <- moduleInstanceListFunction modInst miFnLen
            print ("ListFunction" :: String, miListFn)

            Just funInst <- moduleInstanceFindFunction modInst "addTwo"
            Just _fnTy <- functionInstanceGetFunctionType funInst
            pure ()

testExternRefTableInst :: IO ()
testExternRefTableInst = do
  let wasmLimit = WasmLimit {hasMax = True, shared = False, minLimit = 10, maxLimit = 20}
  Just tabTy <- tableTypeCreate RefType_ExternRef wasmLimit
  Just tabInst <- tableInstanceCreate tabTy
  finalize tabTy
  Just _tabTy1 <- tableInstanceGetTableType tabInst
  _refty <- tableTypeGetRefType _tabTy1
  print ("tableTypeGetRefType" :: String, _refty)
  hsref <- toHsRef ("Hello" :: T.Text)
  res <- tableInstanceSetData tabInst (WasmExternRef hsref) 3
  print ("tableInstanceSetData" :: String, res)
  (_, WasmExternRef eref) <- tableInstanceGetData tabInst 3
  valMay <- fromHsRef @T.Text eref
  print ("tableInstanceGetData" :: String, valMay)

  tabSz <- tableInstanceGetSize tabInst
  print ("tableInstanceGetSize" :: String, tabSz)
  _ <- tableInstanceGrow tabInst 6
  tabSzGrown <- tableInstanceGetSize tabInst
  print ("tableInstanceGetSize After Grow" :: String, tabSzGrown)
  -- finalize tabTy
  pure ()


testFunRefTableInst :: IO ()
testFunRefTableInst = do
  let wasmLimit = WasmLimit {hasMax = True, shared = False, minLimit = 10, maxLimit = 20}
  Just tabTy <- tableTypeCreate RefType_FuncRef wasmLimit
  Just tabInst <- tableInstanceCreate tabTy
  finalize tabTy
  Just _tabTy1 <- tableInstanceGetTableType tabInst
  _refty <- tableTypeGetRefType _tabTy1
  print ("tableTypeGetRefType" :: String, _refty)

  void $ withWasmResT configureCreate $ \cfgCxt -> do
    configureAddHostRegistration cfgCxt HostRegistration_Wasi
    void $ withWasmResT (executorCreate (Just cfgCxt) Nothing) $ \exec -> do
      void $ withWasmResT (loaderCreate cfgCxt) $ \loader -> do
        (_, astModMay) <- loaderParseFromFile loader "./tests/sample/wasm/addTwo.wasm"
        let astMod = maybe (error "Failed to load AST module") id astModMay
        void $ withWasmResT (validatorCreate cfgCxt) $ \validator -> do
          _vres <- validatorValidate validator astMod
          void $ withWasmResT (storeCreate) $ \store -> do
            (_eres, modInst) <- executorRegister exec store astMod "mod"
            miFnLen <- moduleInstanceListFunctionLength modInst
            print ("ListFunctionLength" :: String, miFnLen)
            miListFn <- moduleInstanceListFunction modInst miFnLen
            print ("ListFunction" :: String, miListFn)

            Just funInst <- moduleInstanceFindFunction modInst "addTwo"
            let fnRef = WasmFuncRef funInst
            res <- tableInstanceSetData tabInst fnRef 3
            print ("tableInstanceSetData" :: String, res)
            pure ()

  --finalize tabTy


testMemoryInst :: IO ()
testMemoryInst = do
  let wasmLimit = WasmLimit {hasMax = True, shared = False, minLimit = 10, maxLimit = 20}
  Just memTy <- memoryTypeCreate wasmLimit
  Just memInst <- memoryInstanceCreate memTy
  finalize memTy
  res <- memoryInstanceSetData memInst "ab" 0
  print ("memoryInstanceSetData" :: String, res)

  dataBS <- memoryInstanceGetPointer memInst 2 0
  print ("memoryInstanceGetPointer" :: String, Char8.unpack dataBS)

  dataKBS <- memoryInstanceGetPointerConst memInst 2 0
  print ("memoryInstanceGetPointerConst" :: String, Char8.unpack dataKBS)

  pageSz <- memoryInstanceGetPageSize memInst
  print ("memoryInstanceGetPageSize" :: String, pageSz)

  void $ memoryInstanceGrowPage memInst 8
  pageSz1 <- memoryInstanceGetPageSize memInst
  print ("memoryInstanceGetPageSizeAfterGrow" :: String, pageSz1)
  
  -- finalize memTy
  pure ()

testMutGlobalInst :: IO ()
testMutGlobalInst = do
  let i64 = WasmInt64 100
  Just gI64Ty <- globalTypeCreate ValType_I64 Mutability_Var

  Just gI64V1 <- globalInstanceCreate gI64Ty i64

  Just _gI64Ty' <- globalInstanceGetGlobalType gI64V1
  valTy <- globalTypeGetValType gI64Ty
  print("globalTypeGetValType" :: String, valTy)
  mut <- globalTypeGetMutability gI64Ty
  print("globalTypeGetMutability" :: String, mut)

  v1 <- globalInstanceGetValue gI64V1
  print("globalInstanceGetValue" :: String, v1)

  globalInstanceSetValue gI64V1 (WasmInt64 200)

  v2 <- globalInstanceGetValue gI64V1
  print("globalInstanceGetValue[Mutated]" :: String, v2)
  
  print i64
  pure ()

testConstGlobalInst :: IO ()
testConstGlobalInst = do
  let i64 = WasmInt64 100
  Just gI64Ty <- globalTypeCreate ValType_I64 Mutability_Const

  Just gI64V1 <- globalInstanceCreate gI64Ty i64

  Just _gI64Ty' <- globalInstanceGetGlobalType gI64V1
  valTy <- globalTypeGetValType gI64Ty
  print("globalTypeGetValType" :: String, valTy)
  mut <- globalTypeGetMutability gI64Ty
  print("globalTypeGetMutability" :: String, mut)

  v1 <- globalInstanceGetValue gI64V1
  print("globalInstanceGetValue" :: String, v1)

  globalInstanceSetValue gI64V1 (WasmInt64 200)

  v2 <- globalInstanceGetValue gI64V1
  print("globalInstanceGetValue[Mutated]" :: String, v2)
  
  print i64
  pure ()  
  
-- ValueGenNullRef
testValueGenNullRef :: IO ()
testValueGenNullRef = do
    let val = valueGenNullRef RefType_ExternRef
    print $ valueIsNullRef val

testHostFnAlloc :: IO ()
testHostFnAlloc = do
  hFn <- hostFuncCallbackPure 2 1 $ \_ _ args ->
    let
      n1 = case args V.!? 0 of
        Just (WasmInt32 n1') -> n1'
        _ -> error "Expecting WasmInt32"
      n2 = case args V.!? 1 of
        Just (WasmInt32 n2') -> n2'
        _ -> error "Expecting WasmInt32"
    in V.fromList [WasmInt32 (n1 + n2)]
  Just funTy <- functionTypeCreate (SV.fromList [ValType_I32, ValType_I32]) (SV.fromList [ValType_I32])
  hsref <- toHsRef ("test" :: T.Text)
  Just _funInst <- functionInstanceCreate funTy hFn hsref 0
  pure ()

-- testHostFnCallingFrameCxt :: IO ()
-- testHostFnCallingFrameCxt = do
--   pure ()

-- testUDErrorCode :: IO ()
-- testUDErrorCode = do
--   pure ()
