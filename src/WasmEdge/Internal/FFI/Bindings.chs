{-|
Module      : WasmEdge.Internal.FFI.Bindings
Description : Haskell bindings for wasmedge runtime hosting
Copyright   : (c) ByteAlly, 2023
License     : BSD-3
Author      : Magesh B
Maintainer  : magesh85@gmail.com
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module WasmEdge.Internal.FFI.Bindings
  ( HsRef
  , HsRefPtr, withHsRefPtr
  , HasFinalizer
  , WasmVal ( WasmInt32, WasmInt64, WasmFloat, WasmDouble, WasmInt128, WasmExternRef
            , WasmFuncRef, WasmNullExternRef, WasmNullFuncRef
            )
  , WasmString
  , WasmResult
  , Limit (WasmLimit, hasMax, shared, minLimit, maxLimit)
  , ProgramOption, withProgramOption
  , ModuleDescriptor, withModuleDescriptor
  , PluginVersionData, withPluginVersionData
  , PluginDescriptor, withPluginDescriptor
  , ConfigureContext 
  , StatisticsContext
  , ASTModuleContext
  , FunctionTypeContext
  , MemoryTypeContext
  , TableTypeContext
  , GlobalTypeContext
  , ImportTypeContext
  , ExportTypeContext
  , CompilerContext
  , LoaderContext
  , ValidatorContext
  , ExecutorContext
  , StoreContext
  , ModuleInstanceContext
  , FunctionInstanceContext
  , TableInstanceContext
  , MemoryInstanceContext
  , GlobalInstanceContext
  , CallingFrameContext
  , Async
  , VMContext
  , PluginContext
  , HostFuncT
  , versionGet
  , versionGetMajor
  , versionGetMinor
  , versionGetPatch
  , valueGenI32
  , valueGetI32
  , valueGenI64
  , valueGetI64
  , valueGenF32
  , valueGetF32
  , valueGenF64
  , valueGetF64
  , valueGenV128
  , valueGetV128
  , valueGenNullRef
  , valueIsNullRef
  , stringWrap
  , wasmStringEq
  , mkResultSuccess
  , mkResultTerminate 
  , mkResultFail
  , resultOK
  , resultGen 
  , resultGetCode
  , resultGetCategory
  , resultGetMessage
  , valueGenFuncRef 
  , valueGetFuncRef
  , valueGenExternRef 
  , valueGetExternRef 
  , logSetErrorLevel 
  , logSetDebugLevel
  , logOff
  , configureCreate 
  , configureAddProposal
  , configureRemoveProposal
  , configureHasProposal
  , configureAddHostRegistration
  , configureRemoveHostRegistration
  , configureHasHostRegistration
  , configureSetMaxMemoryPage
  , configureGetMaxMemoryPage
  , configureSetForceInterpreter
  , configureIsForceInterpreter
  , configureCompilerSetOptimizationLevel
  , configureCompilerGetOptimizationLevel
  , configureCompilerSetOutputFormat
  , configureCompilerGetOutputFormat
  , configureCompilerSetDumpIR
  , configureCompilerIsDumpIR
  , configureCompilerSetGenericBinary
  , configureCompilerIsGenericBinary
  , configureCompilerSetInterruptible
  , configureCompilerIsInterruptible
  , configureStatisticsSetInstructionCounting
  , configureStatisticsIsInstructionCounting
  , configureStatisticsSetCostMeasuring
  , configureStatisticsIsCostMeasuring
  , configureStatisticsSetTimeMeasuring
  , configureStatisticsIsTimeMeasuring
  , statisticsCreate
  , statisticsGetInstrCount
  , statisticsGetInstrPerSecond
  , statisticsGetTotalCost
  , statisticsSetCostTable
  , statisticsSetCostLimit
  , statisticsClear
  , astModuleListImportsLength
  , astModuleListImports
  , astModuleListExportsLength
  , astModuleListExports
  , functionTypeCreate
  , functionTypeGetParametersLength
  , functionTypeGetParameters
  , functionTypeGetReturnsLength
  , functionTypeGetReturns
  , tableTypeCreate 
  , tableTypeGetRefType
  , memoryTypeCreate
  , memoryTypeGetLimit
  , globalTypeCreate
  , globalTypeGetValType
  , globalTypeGetMutability
  , importTypeGetModuleName
  , importTypeGetExternalName
  , importTypeGetFunctionType
  , importTypeGetTableType
  , importTypeGetMemoryType
  , importTypeGetGlobalType
  , exportTypeGetExternalType
  , exportTypeGetExternalName
  , exportTypeGetFunctionType
  , exportTypeGetTableType
  , exportTypeGetMemoryType
  , exportTypeGetGlobalType
  , compilerCreate
  , compilerCompile
  , compilerCompileFromBuffer
  , loaderCreate
  , loaderParseFromFile
  , loaderParseFromBuffer
  , validatorCreate
  , validatorValidate
  , executorCreate
  , executorInstantiate
  , executorRegister
  , executorRegisterImport
  , executorInvoke
  , executorAsyncInvoke
  , functionInstanceCreate
  , functionInstanceGetFunctionType 
  , tableInstanceCreate
  , tableInstanceGetTableType
  , tableInstanceGetData
  , tableInstanceSetData
  , tableInstanceGetSize
  , tableInstanceGrow
  , memoryInstanceCreate
  , memoryInstanceGetMemoryType
  , memoryInstanceGetData
  , memoryInstanceSetData
  , memoryInstanceGetPointer
  , memoryInstanceGetPointerConst
  , memoryInstanceGetPageSize
  , memoryInstanceGrowPage
  , globalInstanceCreate
  , globalInstanceGetGlobalType
  , globalInstanceGetValue
  , globalInstanceSetValue
  , callingFrameGetExecutor
  , callingFrameGetModuleInstance
  , callingFrameGetMemoryInstance
  , asyncWait
  , asyncWaitFor
  , asyncCancel
  , asyncGetReturnsLength
  , asyncGet
  , vMCreate
  , vMRegisterModuleFromFile
  , vMRunWasmFromFile
  , vMRunWasmFromBuffer
  , vMRunWasmFromASTModule
  , vMAsyncRunWasmFromFile
  , vMAsyncRunWasmFromASTModule
  , vMAsyncRunWasmFromBuffer
  , vMRegisterModuleFromBuffer
  , vMRegisterModuleFromASTModule
  , vMRegisterModuleFromImport
  , vMLoadWasmFromFile
  , vMLoadWasmFromBuffer
  , vMLoadWasmFromASTModule
  , vMValidate
  , vMInstantiate
  , vMExecute
  , vMExecuteRegistered
  , vMAsyncExecute
  , vMAsyncExecuteRegistered
  , vMGetFunctionType
  , vMGetFunctionTypeRegistered
  , vMCleanup
  , vMGetFunctionListLength
  , vMGetFunctionList
  , vMGetImportModuleContext
  , vMGetActiveModule
  , vMGetRegisteredModule
  , vMListRegisteredModuleLength
  , vMListRegisteredModule
  , vMGetStoreContext
  , vMGetLoaderContext
  , vMGetValidatorContext
  , vMGetExecutorContext
  , vMGetStatisticsContext
  , pluginLoadWithDefaultPaths 
  , pluginLoadFromPath 
  , pluginListPluginsLength 
  , pluginListPlugins 
  , pluginFind 
  , pluginGetPluginName
  , pluginListModuleLength 
  , pluginListModule 
  , pluginCreateModule 
  -- ,pluginGetDescriptor -- TODO: Getting undefined ref to 'WasmEdge_Plugin_GetDescriptor'
  --enum
  , ProgramOptionType (..)
  , Proposal  (..)
  , HostRegistration (..)
  , CompilerOptimizationLevel (..)
  , CompilerOutputFormat (..)
  , ErrCategory (..)
  , ErrCode (..)
  , ValType (..)
  , NumType (..)
  , RefType (..)
  , Mutability (..)
  , ExternalType (..)
  --Haskell funcs
  , withWasmRes
  , withWasmResF
  , withWasmResT
  , storeCreate 
  , storeFindModule
  , storeListModuleLength
  , storeListModule
  , moduleInstanceCreate
  , moduleInstanceCreateWithData
  , moduleInstanceCreateWASI
  , moduleInstanceInitWASI
  , moduleInstanceWASIGetExitCode
  , moduleInstanceWASIGetNativeHandler
  , moduleInstanceInitWasmEdgeProcess
  , moduleInstanceGetModuleName
  , moduleInstanceGetHostData
  , moduleInstanceFindFunction
  , moduleInstanceFindTable
  , moduleInstanceFindMemory
  , moduleInstanceFindGlobal
  , moduleInstanceListFunctionLength
  , moduleInstanceListFunction
  , moduleInstanceListTableLength
  , moduleInstanceListTable
  , moduleInstanceListMemoryLength 
  , moduleInstanceListMemory
  , moduleInstanceListGlobalLength 
  , moduleInstanceListGlobal
  , moduleInstanceAddFunction 
  , moduleInstanceAddTable
  , moduleInstanceAddMemory 
  , moduleInstanceAddGlobal
  , tableTypeGetLimit
  , hostFuncCallbackPure
  , hostFuncCallback
  , driverCompiler
  , driverTool
  , driverUniTool
  , fromHsRef
  , toHsRef
  , stringCreateByCString
  , stringCreateByBuffer
  , finalize
  , getValType
  , stringCopy
  , wasmStringLength
  , toText
    -- * Re-exports
  , Int128
  #if TESTONLY && !(__HADDOCK_VERSION__)
  , testonly_getOwner
  , OwnedBy (..)
  #endif
  ) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Word
import Data.List (intercalate)
-- import Foreign
import Foreign.C
-- import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (Storable (..))
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe
-- import GHC.Ptr
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Bifunctor
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Unsafe as UnsafeBS
import qualified Data.ByteString.Internal as IntBS
import qualified Data.Vector as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Coerce
import Control.Arrow ((&&&))
import Data.WideWord.Int128
import GHC.Stable
import GHC.Fingerprint
import GHC.TypeLits
import Data.Typeable
#if TESTONLY
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.MVar
#endif

#include "wasmedge/wasmedge.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>  

{#context prefix = "WasmEdge"#}

#if TESTONLY && !(__HADDOCK_VERSION__)

data RefStore = RefStore !(Set String) !(Set String)
testonly_ownership :: MVar RefStore
testonly_ownership = unsafePerformIO $ newMVar (RefStore mempty mempty)
{-# NOINLINE testonly_ownership #-}

testonly_hsOwned :: Ptr a -> IO ()
testonly_hsOwned p = do
  let !addr = show p
  modifyMVar_ testonly_ownership $ \(RefStore hsSet cSet) -> pure $! (RefStore (Set.insert addr hsSet) cSet)

testonly_cOwned :: Ptr a -> IO ()
testonly_cOwned p = do
  let !addr = show p
  modifyMVar_ testonly_ownership $ \(RefStore hsSet cSet) -> pure $! RefStore hsSet (Set.insert addr cSet)

testonly_getOwner :: HasFinalizer t => t -> IO (Maybe OwnedBy)
testonly_getOwner t = do
  RefStore hsSet cSet <- readMVar testonly_ownership
  addrStr <- withForeignPtr (unsafeToFP t) (pure . show)
  case (Set.member addrStr hsSet, Set.member addrStr cSet) of
    (True, False) -> pure $ Just HsOwned
    (False, True) -> pure $ Just COwned
    (True, True) -> error "Panic: Can't be both HsOwned & COwned"
    (False, False) -> pure Nothing
  where
    unsafeToFP :: t -> ForeignPtr t
    unsafeToFP = unsafeCoerce

data OwnedBy = HsOwned | COwned
  deriving (Show, Eq)
  
#endif

#c

void StringCreateByBufferOut(WasmEdge_String* strOut, const char *Str, const uint32_t Len)
{
  *strOut = WasmEdge_StringCreateByBuffer(Str, Len);
}

void StringWrapOut(WasmEdge_String* strOut, const char *Buf, const uint32_t Len)
{
  *strOut = WasmEdge_StringWrap(Buf, Len);
}

void StringDeleteByPtr(WasmEdge_String* Str)
{
  printf("string is getting deleted\n");
  WasmEdge_StringDelete(*Str);
}

void ResultGenOut(WasmEdge_Result* out, const enum WasmEdge_ErrCategory Category, const uint32_t Code)
{
  *out = WasmEdge_ResultGen(Category, Code);
}

void C_Result_Success(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Success.Code;}
void C_Result_Terminate(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Terminate.Code;}
void C_Result_Fail(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Fail.Code;}

/*
typedef struct U128 {
  uint64_t High;
  uint64_t Low;
} U128;
*/

#if WORDS_BIGENDIAN
#define W128IX0 1
#define W128IX1 0
#else
#define W128IX0 0
#define W128IX1 1
#endif  

typedef struct WasmVal {
  uint64_t Val[2];
  enum WasmEdge_ValType Type;
} WasmVal;

typedef struct HsRef {
  uint64_t Fingerprint[2];
  void* Ref;
} HsRef;

WasmEdge_Value fromWasmVal(const WasmVal* valPtr)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  uint128_t v;
  memcpy(&v, valPtr->Val, 16);
  return (WasmEdge_Value){.Value = v, .Type = valPtr->Type};
#else
  const uint64_t* u128t = valPtr->Val;
  uint128_t v = (uint128_t) {.High = u128t[W128IX1], .Low = u128t[W128IX0]};
  return (WasmEdge_Value){.Value = v, .Type = valPtr->Type};
#endif
}

void toWasmVal(WasmVal* valPtr, const WasmEdge_Value val)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  memcpy(valPtr->Val, &val.Value, 16);
  valPtr->Type = val.Type;
#else
  uint64_t u128[2];
  u128[W128IX0] = val.Value.Low;
  u128[W128IX1] = val.Value.High;
  memcpy(valPtr->Val, u128, 16);
  valPtr->Type = val.Type;
#endif
}


uint128_t pack_uint128_t(const uint64_t* u128)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  uint128_t v;
  memcpy(&v, u128, 16);
  return v;
#else
  return (uint128_t) {.High = u128[W128IX1], .Low = u128[W128IX0]};
#endif
}

int128_t pack_int128_t(const uint64_t* u128)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  int128_t v;
  memcpy(&v, u128, 16);
  return v;    
#else
  return (int128_t) {.High = u128[W128IX1], .Low = u128[W128IX0]};
#endif
}

void unpack_int128_t(uint64_t* u128, const int128_t i128)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  memcpy(u128, &i128, 16);    
#else
  u128[W128IX0] = i128.Low;
  u128[W128IX1] = i128.High;
#endif
}

void unpack_uint128_t(uint64_t* u128, const uint128_t ui128)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  memcpy(u128, &ui128, 16);
#else
  u128[W128IX0] = u128.Low;
  u128[W128IX1] = u128.High;
#endif
}

void ValueGenI32(WasmVal* valOut, const int32_t Val)
{ WasmEdge_Value r = WasmEdge_ValueGenI32(Val);
  toWasmVal(valOut, r);
}

int32_t ValueGetI32 (const WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetI32(val);
}

void ValueGenI64(WasmVal* valOut, const int64_t Val)
{ WasmEdge_Value r = WasmEdge_ValueGenI64(Val);
  toWasmVal(valOut, r);
}

int64_t ValueGetI64 (const WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetI64(val);
}

void ValueGenF32(WasmVal* valOut, const float Val)
{ WasmEdge_Value r = WasmEdge_ValueGenF32(Val);
  toWasmVal(valOut, r);
}

float ValueGetF32 (const WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetF32(val);
}

void ValueGenF64(WasmVal* valOut, const double Val)
{ WasmEdge_Value r = WasmEdge_ValueGenF64(Val);
  toWasmVal(valOut, r);
}

double ValueGetF64 (const WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetF64(val);
}

void ValueGenV128(WasmVal* valOut, const uint64_t* Val)
{ WasmEdge_Value r = WasmEdge_ValueGenV128(pack_int128_t(Val));
  toWasmVal(valOut, r);
}

void ValueGetV128 (const WasmVal* v, uint64_t* u128Out)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  unpack_int128_t(u128Out, WasmEdge_ValueGetV128(val));
}

void ValueGenNullRef(WasmVal* valOut, const enum WasmEdge_RefType T)
{ WasmEdge_Value r = WasmEdge_ValueGenNullRef(T);
  toWasmVal(valOut, r);
}

bool ValueIsNullRef (WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueIsNullRef(val);
}

void ValueGenFuncRef(WasmVal* valOut, const WasmEdge_FunctionInstanceContext *Cxt)
{ WasmEdge_Value r = WasmEdge_ValueGenFuncRef(Cxt);
  toWasmVal(valOut, r);
}

const WasmEdge_FunctionInstanceContext * ValueGetFuncRef (const WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetFuncRef(val);
}

void ValueGenExternRef(WasmVal* valOut, HsRef *hsRef)
{ WasmEdge_Value r = WasmEdge_ValueGenExternRef(hsRef);
  toWasmVal(valOut, r);
}

const HsRef * ValueGetExternRef (const WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return (HsRef *) WasmEdge_ValueGetExternRef(val);
}

void StringCreateByCStringOut(WasmEdge_String *strOut,const char* Str){ *strOut = WasmEdge_StringCreateByCString(Str); }
void ImportTypeGetModuleNameOut(WasmEdge_String* strOut,WasmEdge_ImportTypeContext* Ctx){ *strOut = WasmEdge_ImportTypeGetModuleName(Ctx); }
void ImportTypeGetExternalNameOut(WasmEdge_String* strOut,WasmEdge_ImportTypeContext* Ctx){ *strOut = WasmEdge_ImportTypeGetExternalName(Ctx); }
void ExportTypeGetExternalNameOut(WasmEdge_String* strOut,WasmEdge_ExportTypeContext* Ctx){ *strOut = WasmEdge_ExportTypeGetExternalName(Ctx); }
void CompilerCompileOut(WasmEdge_Result* resOut,WasmEdge_CompilerContext* Ctx,const char* InPath,const char* OutPath){ *resOut = WasmEdge_CompilerCompile(Ctx,InPath,OutPath); }
void CompilerCompileFromBufferOut(WasmEdge_Result* resOut,WasmEdge_CompilerContext* Ctx,const uint8_t *InBuffer,const uint64_t InBufferLen,const char *OutPath)
{ 
  *resOut = WasmEdge_CompilerCompileFromBuffer(Ctx,InBuffer,InBufferLen,OutPath); 
}
void LoaderParseFromFileOut(WasmEdge_Result* resOut,WasmEdge_LoaderContext *Cxt, WasmEdge_ASTModuleContext **Module, const char *Path)
{
  *resOut = WasmEdge_LoaderParseFromFile(Cxt, Module, Path);
}

void LoaderParseFromBufferOut(WasmEdge_Result* resOut,WasmEdge_LoaderContext *Cxt, WasmEdge_ASTModuleContext **Module, const uint8_t *Buf, const uint32_t BufLen)
{
  *resOut = WasmEdge_LoaderParseFromBuffer(Cxt, Module, Buf, BufLen);
}

void ValidatorValidateOut(WasmEdge_Result* resOut,WasmEdge_ValidatorContext* Ctx,const WasmEdge_ASTModuleContext *ASTCxt){ *resOut = WasmEdge_ValidatorValidate(Ctx,ASTCxt); }

void ExecutorInstantiateOut(WasmEdge_Result* resOut, WasmEdge_ExecutorContext *Cxt, WasmEdge_ModuleInstanceContext **ModuleCxt, WasmEdge_StoreContext *StoreCxt, const WasmEdge_ASTModuleContext *ASTCxt)
{
  *resOut = WasmEdge_ExecutorInstantiate(Cxt, ModuleCxt, StoreCxt, ASTCxt);
}

void ExecutorRegisterOut(WasmEdge_Result* resOut, WasmEdge_ExecutorContext *Cxt, WasmEdge_ModuleInstanceContext **ModuleCxt, WasmEdge_StoreContext *StoreCxt, const WasmEdge_ASTModuleContext *ASTCxt,WasmEdge_String ModuleName)
{
  *resOut = WasmEdge_ExecutorRegister(Cxt, ModuleCxt, StoreCxt, ASTCxt, ModuleName);
}

void TableTypeGetLimitOut(WasmEdge_Limit* limOut,const WasmEdge_TableTypeContext *Cxt){ *limOut = WasmEdge_TableTypeGetLimit(Cxt); }
void MemoryTypeGetLimitOut(WasmEdge_Limit* limOut,const WasmEdge_MemoryTypeContext *Cxt)
{
  *limOut = WasmEdge_MemoryTypeGetLimit(Cxt); 
}
void ExecutorRegisterImportOut(WasmEdge_Result* resOut, WasmEdge_ExecutorContext *Cxt, WasmEdge_StoreContext *StoreCxt,const WasmEdge_ModuleInstanceContext *ImportCxt){ 
*resOut = WasmEdge_ExecutorRegisterImport(Cxt,StoreCxt,ImportCxt); }

void ExecutorInvokeOut(WasmEdge_Result *resOut, WasmEdge_ExecutorContext *Cxt,const WasmEdge_FunctionInstanceContext *FuncCxt,
  const WasmVal **WValParams, const uint32_t ParamLen,WasmVal **ReturnsOut, const uint32_t ReturnLen)
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  WasmEdge_Value *Returns = (WasmEdge_Value *)malloc(ReturnLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }

  *resOut = WasmEdge_ExecutorInvoke (Cxt,FuncCxt,Params,ParamLen,Returns,ReturnLen);

  for (int i = 0; i < ReturnLen; i++)
  {
    WasmVal* pRes = ReturnsOut[i];
    toWasmVal(pRes, Returns[i]);
  }
  free(Params);
  free(Returns);
}

WasmEdge_Async *ExecutorAsyncInvokeOut(WasmEdge_ExecutorContext *Cxt,const WasmEdge_FunctionInstanceContext *FuncCxt,const WasmVal **WValParams,const uint32_t ParamLen)
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }
  WasmEdge_Async *res = WasmEdge_ExecutorAsyncInvoke(Cxt,FuncCxt,Params,ParamLen);
  free(Params);
  return res;
}

void ModuleInstanceGetModuleNameOut(WasmEdge_String* strOut,WasmEdge_ModuleInstanceContext* Ctx){ *strOut = WasmEdge_ModuleInstanceGetModuleName(Ctx); }

typedef WasmEdge_Result* (*HostFunc_t)(
    HsRef *Data, const WasmEdge_CallingFrameContext *CallFrameCxt,
    WasmVal **Params, WasmVal **Returns);

typedef struct HostFuncClosure {
  HsRef* Data;
  uint32_t ParLen;
  uint32_t RetLen;
  HostFunc_t HostFunc;
} HostFuncClosure;
  

WasmEdge_Result cbHostFunc_t (void *Data, const WasmEdge_CallingFrameContext *CallFrameCxt,const WasmEdge_Value *Params, WasmEdge_Value *Returns)
{
  HostFuncClosure* clsr = (HostFuncClosure*)Data;
  HostFunc_t HostFunc = clsr->HostFunc;
  uint32_t parLen = clsr->ParLen;
  uint32_t retLen = clsr->RetLen;
  WasmVal **params = (WasmVal **)malloc(parLen * sizeof(WasmVal *));
  WasmVal **returns = (WasmVal **)malloc(retLen * sizeof(WasmVal *));
  for(int i = 0; i < parLen; i++)
  {
    WasmEdge_Value r = Params[i];
    WasmVal *pOut = (WasmVal*)malloc(sizeof(WasmVal));
    toWasmVal(pOut, r);
    params[i] = pOut;
  }
  WasmEdge_Result* resPtr = HostFunc(clsr->Data, CallFrameCxt, params, returns);
  // free params array
  free(params);
  for (int i = 0; i <- retLen; i++)
  {
    const WasmVal *rOut = returns[i];
    WasmEdge_Value rval = {.Value = pack_uint128_t(rOut->Val), .Type = rOut->Type};
    Returns[i] = rval;
  }
  // free returns array
  free(returns);
  free(clsr);
  return (*resPtr);
}

WasmEdge_FunctionInstanceContext * FunctionInstanceCreateBndr(const WasmEdge_FunctionTypeContext *Type, HostFunc_t HostFunc, HsRef* hsRef, const uint64_t Cost)
{
  uint32_t parLen = WasmEdge_FunctionTypeGetParametersLength(Type);
  uint32_t retLen = WasmEdge_FunctionTypeGetReturnsLength(Type);
  HostFuncClosure* clsr = (HostFuncClosure*) malloc(sizeof(HostFuncClosure));
  clsr->Data = hsRef;
  clsr->ParLen = parLen;
  clsr->RetLen = retLen;
  clsr->HostFunc = HostFunc;
  WasmEdge_FunctionInstanceCreate(Type, cbHostFunc_t, clsr, Cost);
}

/* TODO: Required only for completion
typedef WasmEdge_Result (*WrapFunc_t)(
    void *This, HsRef *Data, const WasmEdge_CallingFrameContext *CallFrameCxt,
    WasmVal **Params, const uint32_t ParamLen,
    WasmVal **Returns, const uint32_t ReturnLen);
*/  

void TableInstanceGetDataOut(WasmEdge_Result* resOut,const WasmEdge_TableInstanceContext *Cxt,WasmVal *v, const uint32_t Offset)
{
   WasmEdge_Value* Data = (WasmEdge_Value*)malloc(sizeof(WasmEdge_Value));
   *resOut = WasmEdge_TableInstanceGetData(Cxt,Data,Offset);
   toWasmVal(v, *Data);
   free(Data);
}
void TableInstanceSetDataOut(WasmEdge_Result *resOut,WasmEdge_TableInstanceContext *Cxt,WasmVal *v, const uint32_t Offset)
{
  WasmEdge_Value Data = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  *resOut = WasmEdge_TableInstanceSetData(Cxt,Data,Offset);
}
void TableInstanceGrowOut(WasmEdge_Result* resOut,WasmEdge_TableInstanceContext* Ctx,const uint32_t Size){ *resOut = WasmEdge_TableInstanceGrow(Ctx,Size); }
void MemoryInstanceGetDataOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext* Ctx,uint8_t *Data,const uint32_t Length, const uint32_t Offset){ 
*resOut = WasmEdge_MemoryInstanceGetData(Ctx,Data,Offset,Length); }
void MemoryInstanceSetDataOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext* Ctx,uint8_t *Data,const uint32_t Length, const uint32_t Offset){ 
*resOut = WasmEdge_MemoryInstanceSetData(Ctx,Data,Offset,Length); }
void MemoryInstanceGrowPageOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext *Cxt,const uint32_t Page){ *resOut = WasmEdge_MemoryInstanceGrowPage(Cxt,Page); }
WasmEdge_GlobalInstanceContext* GlobalInstanceCreateOut (const WasmEdge_GlobalTypeContext *GlobType,WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_GlobalInstanceCreate(GlobType,val); 
}
void GlobalInstanceGetValueOut(WasmVal *v,const WasmEdge_GlobalInstanceContext *Cxt)
{
    WasmEdge_Value val = WasmEdge_GlobalInstanceGetValue(Cxt);
    toWasmVal(v, val);
}
void GlobalInstanceSetValueOut(WasmEdge_GlobalInstanceContext *Cxt,const WasmVal *v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  WasmEdge_GlobalInstanceSetValue(Cxt,val);
}
void AsyncGetOut(WasmEdge_Result *resOut,const WasmEdge_Async *Cxt, WasmVal **ReturnsOut,const uint32_t ReturnLen)
{
   WasmEdge_Value *Returns = (WasmEdge_Value *)malloc(ReturnLen * sizeof(WasmEdge_Value));
   *resOut = WasmEdge_AsyncGet(Cxt,Returns,ReturnLen);
   for (int i = 0; i < ReturnLen; i++)
   {
    WasmVal* pRes = ReturnsOut[i];
    toWasmVal(pRes, Returns[i]);
   }
   free(Returns);
}
void VMRunWasmFromFileOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const char *Path, const WasmEdge_String FuncName, const WasmVal **WValParams, const uint32_t ParamLen,WasmVal **ReturnsOut, const uint32_t ReturnLen) 
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  WasmEdge_Value *Returns = (WasmEdge_Value *)malloc(ReturnLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }

  *resOut = WasmEdge_VMRunWasmFromFile(Cxt,Path,FuncName,Params,ParamLen,Returns,ReturnLen);

  for (int i = 0; i < ReturnLen; i++)
  {
    WasmVal* pRes = ReturnsOut[i];
    toWasmVal(pRes, Returns[i]);
  }
  free(Params);
  free(Returns);
}
void VMRunWasmFromASTModuleOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_ASTModuleContext *ASTCxt,const WasmEdge_String FuncName, const WasmVal **WValParams,const uint32_t ParamLen,WasmVal **ReturnsOut, const uint32_t ReturnLen)
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  WasmEdge_Value *Returns = (WasmEdge_Value *)malloc(ReturnLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }

  *resOut = WasmEdge_VMRunWasmFromASTModule(Cxt,ASTCxt,FuncName,Params,ParamLen,Returns,ReturnLen);

  for (int i = 0; i < ReturnLen; i++)
  {
    WasmVal* pRes = ReturnsOut[i];
    toWasmVal(pRes, Returns[i]);
  }
  free(Params);
  free(Returns);
}

void AsyncFinalize(WasmEdge_Async *Cxt)
{
  printf("Before AsyncFinalize\n");
  // WasmEdge_AsyncCancel(Cxt);
  WasmEdge_AsyncDelete(Cxt);
  printf("After AsyncFinalize\n");
}

WasmEdge_Async *VMAsyncRunWasmFromFileOut(WasmEdge_VMContext *Cxt, const char *Path, const WasmEdge_String FuncName,const WasmVal **WValParams, const uint32_t ParamLen)
{
 WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }
  printf("Before AsyncRun\n");
  WasmEdge_Async *res = WasmEdge_VMAsyncRunWasmFromFile(Cxt,Path,FuncName,Params,ParamLen);
  printf("After AsyncRun\n");
  free(Params);
  return res;
}

WasmEdge_Async *VMAsyncRunWasmFromASTModuleOut(WasmEdge_VMContext *Cxt,const WasmEdge_ASTModuleContext *ASTCxt,const WasmEdge_String FuncName,const WasmVal **WValParams,const uint32_t ParamLen)
{
 WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }
  WasmEdge_Async *res = WasmEdge_VMAsyncRunWasmFromASTModule(Cxt,ASTCxt,FuncName,Params,ParamLen);
  free(Params);
  return res;
}

void VMExecuteOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_String FuncName,const WasmVal **WValParams, const uint32_t ParamLen,WasmVal **ReturnsOut, const uint32_t ReturnLen)
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  WasmEdge_Value *Returns = (WasmEdge_Value *)malloc(ReturnLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }

  *resOut = WasmEdge_VMExecute(Cxt,FuncName,Params,ParamLen,Returns,ReturnLen);

  for (int i = 0; i < ReturnLen; i++)
  {
    WasmVal* pRes = ReturnsOut[i];
    toWasmVal(pRes, Returns[i]);
  }
  free(Params);
  free(Returns);
}

void VMExecuteRegisteredOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_String ModuleName,const WasmEdge_String FuncName, const WasmVal **WValParams,const uint32_t ParamLen, WasmVal **ReturnsOut, const uint32_t ReturnLen)
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  WasmEdge_Value *Returns = (WasmEdge_Value *)malloc(ReturnLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }

  *resOut = WasmEdge_VMExecuteRegistered(Cxt,ModuleName,FuncName,Params,ParamLen,Returns,ReturnLen);

  for (int i = 0; i < ReturnLen; i++)
  {
    WasmVal* pRes = ReturnsOut[i];
    toWasmVal(pRes, Returns[i]);
  }
  free(Params);
  free(Returns);
}

WasmEdge_Async *VMAsyncExecuteOut(WasmEdge_VMContext *Cxt, const WasmEdge_String FuncName,const WasmVal **WValParams, const uint32_t ParamLen)
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }
  WasmEdge_Async *res = WasmEdge_VMAsyncExecute(Cxt,FuncName,Params,ParamLen);
  free(Params);
  return res; 
}

WasmEdge_Async* VMAsyncExecuteRegisteredOut(WasmEdge_VMContext *Cxt, const WasmEdge_String ModuleName,const WasmEdge_String FuncName, const WasmVal **WValParams,const uint32_t ParamLen)
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }
  WasmEdge_Async *res = WasmEdge_VMAsyncExecuteRegistered(Cxt,ModuleName,FuncName,Params,ParamLen);
  free(Params);
  return res; 
}

uint32_t PluginListPluginsOut(WasmEdge_String **NamesOut, const uint32_t Len){
  WasmEdge_String *Names = (WasmEdge_String *)malloc(Len * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_PluginListPlugins(Names,Len);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}

uint32_t  PluginListModuleOut(const WasmEdge_PluginContext *Cxt,WasmEdge_String **NamesOut, const uint32_t Len){
  WasmEdge_String *Names = (WasmEdge_String *)malloc(Len * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_PluginListModule(Cxt, Names,Len);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}

uint32_t VMListRegisteredModuleOut(const WasmEdge_VMContext *Cxt,WasmEdge_String **NamesOut, const uint32_t Len){
  WasmEdge_String *Names = (WasmEdge_String *)malloc(Len * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_VMListRegisteredModule(Cxt, Names,Len);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}

uint32_t ModuleInstanceListGlobalOut(const WasmEdge_ModuleInstanceContext *Cxt,WasmEdge_String **NamesOut,const uint32_t Len){
  WasmEdge_String *Names = (WasmEdge_String *)malloc(Len * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_ModuleInstanceListGlobal(Cxt, Names,Len);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}

uint32_t ModuleInstanceListFunctionOut(const WasmEdge_ModuleInstanceContext *Cxt,WasmEdge_String **NamesOut,const uint32_t Len){
  WasmEdge_String *Names = (WasmEdge_String *)malloc(Len * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_ModuleInstanceListFunction(Cxt, Names,Len);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}

uint32_t ModuleInstanceListTableOut( const WasmEdge_ModuleInstanceContext *Cxt,WasmEdge_String **NamesOut,const uint32_t Len  ){
  WasmEdge_String *Names = (WasmEdge_String *)malloc(Len * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_ModuleInstanceListTable(Cxt, Names,Len);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}

uint32_t ModuleInstanceListMemoryOut( const WasmEdge_ModuleInstanceContext *Cxt,WasmEdge_String **NamesOut,const uint32_t Len  ){
  WasmEdge_String *Names = (WasmEdge_String *)malloc(Len * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_ModuleInstanceListMemory(Cxt, Names, Len);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}


uint32_t VMGetFunctionListOut(
    const WasmEdge_VMContext *Cxt, WasmEdge_String **NamesOut, const uint32_t NLen,
    const WasmEdge_FunctionTypeContext **FuncTypes, const uint32_t FTLen)
{
  // TODO: assert(NLen == FTLen);
  WasmEdge_String *Names = (WasmEdge_String *)malloc(FTLen * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_VMGetFunctionList(Cxt, Names, FuncTypes, FTLen);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}

void PluginGetPluginNameOut(WasmEdge_String* strOut,const WasmEdge_PluginContext *Cxt){ *strOut = WasmEdge_PluginGetPluginName(Cxt); }
void VMRegisterModuleFromFileOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt,const WasmEdge_String ModuleName,const char *Path){ *resOut = WasmEdge_VMRegisterModuleFromFile(Cxt,ModuleName,Path); }
void VMInstantiateOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt){ *resOut = WasmEdge_VMInstantiate(Cxt); }
void VMValidateOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt){ *resOut = WasmEdge_VMValidate(Cxt); }
void VMLoadWasmFromASTModuleOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt,const WasmEdge_ASTModuleContext *ASTCxt){ *resOut = WasmEdge_VMLoadWasmFromASTModule(Cxt,ASTCxt); }
void VMLoadWasmFromFileOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt, const char *Path){ *resOut = WasmEdge_VMLoadWasmFromFile(Cxt,Path); }
void VMRegisterModuleFromImportOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt,const WasmEdge_ModuleInstanceContext *ImportCxt){ *resOut = WasmEdge_VMRegisterModuleFromImport(Cxt,ImportCxt); }
void VMRegisterModuleFromASTModuleOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt,const WasmEdge_String ModuleName, const WasmEdge_ASTModuleContext *ASTCxt){ 
*resOut = WasmEdge_VMRegisterModuleFromASTModule(Cxt,ModuleName,ASTCxt); }

WasmEdge_Async* VMAsyncRunWasmFromBufferOut(WasmEdge_VMContext *Cxt, const uint8_t *Buf, const uint32_t BufLen,const WasmEdge_String FuncName,const WasmVal **WValParams,const uint32_t ParamLen )
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }
  WasmEdge_Async *res = WasmEdge_VMAsyncRunWasmFromBuffer(Cxt,Buf,BufLen,FuncName,Params,ParamLen);
  free(Params);
  return res; 
}

void VMLoadWasmFromBufferOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt,const uint8_t *Buf, const uint32_t BufLen){ *resOut = WasmEdge_VMLoadWasmFromBuffer(Cxt,Buf,BufLen); }
void VMRegisterModuleFromBufferOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt,const WasmEdge_String ModuleName,const uint8_t *Buf, const uint32_t BufLen )
{
  *resOut = WasmEdge_VMRegisterModuleFromBuffer(Cxt,ModuleName,Buf,BufLen); 
}

void VMRunWasmFromBufferOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const uint8_t *Buf, const uint32_t BufLen,const WasmEdge_String FuncName, const WasmVal **WValParams,
  const uint32_t ParamLen, WasmVal **ReturnsOut, const uint32_t ReturnLen)
{
  WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  WasmEdge_Value *Returns = (WasmEdge_Value *)malloc(ReturnLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }

  *resOut = WasmEdge_VMRunWasmFromBuffer(Cxt,Buf,BufLen,FuncName,Params,ParamLen,Returns,ReturnLen);

  for (int i = 0; i < ReturnLen; i++)
  {
    WasmVal* pRes = ReturnsOut[i];
    toWasmVal(pRes, Returns[i]);
  }
  free(Params);
  free(Returns);
}

uint32_t StoreListModuleOut(const WasmEdge_StoreContext *Cxt,WasmEdge_String **NamesOut, const uint32_t Len){
  WasmEdge_String *Names = (WasmEdge_String *)malloc(Len * sizeof(WasmEdge_String));
  uint32_t retLen = WasmEdge_StoreListModule(Cxt,Names,Len);
  for(int i=0; i < retLen; i++)
  {
    WasmEdge_String *nameOut = NamesOut[i];
    *nameOut = Names[i];
  }
  free(Names);
  return retLen;
}

int Driver_Compiler(const char *Argv[], int Argc)
{
  WasmEdge_Driver_Compiler(Argc, Argv);
}

int Driver_Tool(const char *Argv[], int Argc)
{
  WasmEdge_Driver_Tool(Argc, Argv);
}

int Driver_UniTool(const char *Argv[], int Argc)
{
  WasmEdge_Driver_UniTool(Argc, Argv);
}
#endc

withWasmRes :: HasFinalizer res => IO res -> (res -> IO r) -> IO r
withWasmRes act f = do
  res <- act
  r <- f res
  finalize res
  pure r

withWasmResF :: (Traversable f, HasFinalizer res) => IO (f res) -> (f res -> IO r) -> IO r
withWasmResF act f = do
  res <- act
  r <- f res
  sequence_ (finalize <$> res)
  pure r

withWasmResT :: (Traversable f, HasFinalizer res) => IO (f res) -> (res -> IO r) -> IO (f r)
withWasmResT act f = withWasmResF act $ \fres -> sequenceA (f <$> fres)

{-|
Get the version string of the WasmEdge C API.
-}
{#fun pure unsafe VersionGet as ^ {} -> `Text' fromCStrToText*#}

{-|
Get the major version value of the WasmEdge C API.
-}
{#fun pure unsafe VersionGetMajor as ^ {} -> `Word' fromIntegral#}

{-|
Get the minor version value of the WasmEdge C API.
-}
{#fun pure unsafe VersionGetMinor as ^ {} -> `Word' fromIntegral#}

{-|
Get the patch version value of the WasmEdge C API.
-}
{#fun pure unsafe VersionGetPatch as ^ {} -> `Word' fromIntegral#}
    

{-|
  HsRef
-}
{#pointer *HsRef as HsRefPtr foreign newtype #}
{-|
 WasmVal
-}
{#pointer *WasmVal as WasmVal foreign newtype #}
{-|
  WasmEdge string struct.
-}
{#pointer *WasmEdge_String as WasmString foreign finalizer StringDeleteByPtr as ^ newtype #}
instance HasFinalizer WasmString where
  getFinalizer = stringDeleteByPtr

{-|
  WasmEdge Result construct
-}
{#pointer *WasmEdge_Result as WasmResult foreign newtype #}

{-|
 Struct of WASM limit.
-}
{#pointer *WasmEdge_Limit as Limit foreign newtype #}

pattern WasmLimit :: Bool -> Bool -> Word32 -> Word32 -> Limit
pattern WasmLimit {hasMax, shared, minLimit, maxLimit} <- ((getLimitHasMax_ &&& getLimitShared_ &&& getLimitMin_ &&& getLimitMax_) -> (hasMax, (shared, (minLimit, maxLimit)))) where
  WasmLimit hasMax shared minLimit maxLimit = mkLimit hasMax shared minLimit maxLimit

{-# COMPLETE WasmLimit #-}  

mkLimit :: Bool -> Bool -> Word32 -> Word32 -> Limit
mkLimit hasMax' shared' minLimit' maxLimit'  = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes {#sizeof WasmEdge_Limit#}
  withForeignPtr fp $ \p -> do
    {#set WasmEdge_Limit.HasMax#} p hasMax'
    {#set WasmEdge_Limit.Shared#} p shared'
    {#set WasmEdge_Limit.Min#} p (fromIntegral minLimit')
    {#set WasmEdge_Limit.Max#} p (fromIntegral maxLimit')

  pure $ coerce fp

getLimitHasMax_ :: Limit -> Bool
getLimitHasMax_ lmt = unsafePerformIO $ withLimit lmt {#get WasmEdge_Limit.HasMax#}

getLimitShared_ :: Limit -> Bool
getLimitShared_ lmt = unsafePerformIO $ withLimit lmt {#get WasmEdge_Limit.Shared#}

getLimitMin_ :: Limit -> Word32
getLimitMin_ lmt = unsafePerformIO $ withLimit lmt ((fmap fromIntegral) . {#get WasmEdge_Limit.Min#})

getLimitMax_ :: Limit -> Word32
getLimitMax_ lmt = unsafePerformIO $ withLimit lmt ((fmap fromIntegral) . {#get WasmEdge_Limit.Max#})

{- | 
Program option for plugins.
-}
{#pointer *WasmEdge_ProgramOption as ProgramOption foreign newtype #}

{- | Module descriptor for plugins.
-}
{#pointer *WasmEdge_ModuleDescriptor as ModuleDescriptor foreign newtype #}

{- | Version data for plugins.
-}
{#pointer *WasmEdge_PluginVersionData as PluginVersionData foreign newtype #}

{-|
 Plugin descriptor for plugins.
-}
{#pointer *WasmEdge_PluginDescriptor as PluginDescriptor foreign newtype #}

---
foreign import ccall "wrapper" finalizerHSData :: (Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> IO ()))

pattern WasmInt32 :: Int32 -> WasmVal
pattern WasmInt32 i32 <- ((valueGetI32 &&& getValType) -> (i32, ValType_I32)) where
  WasmInt32 i32 = valueGenI32 i32

pattern WasmInt64 :: Int64 -> WasmVal
pattern WasmInt64 i64 <- ((valueGetI64 &&& getValType) -> (i64, ValType_I64)) where
  WasmInt64 i64 = valueGenI64 i64

pattern WasmFloat :: Float -> WasmVal
pattern WasmFloat f32 <- ((valueGetF32 &&& getValType) -> (f32, ValType_F32)) where
  WasmFloat f32 = valueGenF32 f32

pattern WasmDouble :: Double -> WasmVal
pattern WasmDouble f64 <- ((valueGetF64 &&& getValType) -> (f64, ValType_F64)) where
  WasmDouble f64 = valueGenF64 f64

pattern WasmInt128 :: Int128 -> WasmVal
pattern WasmInt128 f64 <- ((valueGetV128 &&& getValType) -> (f64, ValType_V128)) where
  WasmInt128 v128 = valueGenV128 v128

pattern WasmNullExternRef :: WasmVal
pattern WasmNullExternRef <- ((valueIsNullRef &&& getValType) -> (True, ValType_ExternRef)) where
  WasmNullExternRef = valueGenNullRef RefType_ExternRef

pattern WasmNullFuncRef :: WasmVal
pattern WasmNullFuncRef <- ((valueIsNullRef &&& getValType) -> (True, ValType_FuncRef)) where
  WasmNullFuncRef = valueGenNullRef RefType_FuncRef  

pattern WasmExternRef :: HsRef -> WasmVal
pattern WasmExternRef a <- ((valueGetExternRef &&& getValType) -> (a, ValType_ExternRef)) where
  WasmExternRef href = valueGenExternRef href

pattern WasmFuncRef :: FunctionInstanceContext -> WasmVal
pattern WasmFuncRef f <- ((valueGetFuncRef &&& getValType) -> (f, ValType_FuncRef)) where
  WasmFuncRef f = valueGenFuncRef f

{-# COMPLETE WasmInt32, WasmInt64, WasmFloat, WasmDouble, WasmInt128, WasmNullExternRef, WasmNullFuncRef, WasmExternRef, WasmFuncRef #-}  

data HsRef where
  HsRef :: Fingerprint -> StablePtr a -> HsRef

instance Show HsRef where
  show (HsRef fpr _) = show fpr

instance Eq HsRef where
  HsRef fpr1 sp1 == HsRef fpr2 sp2
    | fpr1 == fpr2 = (unsafeCoerce sp1) == sp2
    | otherwise = False

{-|
 To External Reference 
-}
toHsRef :: forall a.Typeable a => a -> IO HsRef
toHsRef a = do
  sp <- newStablePtr a
  pure $ HsRef (typeRepFingerprint $ typeRep (Proxy @a)) sp

{-|
  From External Reference
-}
fromHsRef :: forall a.Typeable a => HsRef -> IO (Maybe a)
fromHsRef (HsRef fpr sp) =
  if typeRepFingerprint (typeRep (Proxy @a)) == fpr
    then fmap Just $ deRefStablePtr @a (coerceSP sp)
    else pure Nothing
  where         
    coerceSP :: forall x. StablePtr x -> StablePtr a
    coerceSP = unsafeCoerce

{-|
  Freeing External Reference
-}
freeHsRef :: HsRef -> IO ()
freeHsRef (HsRef _ sp) = freeStablePtr sp

instance Show WasmVal where
  show = \case
    WasmInt32 v -> show v
    WasmInt64 v -> show v
    WasmFloat v -> show v
    WasmDouble v -> show v
    WasmInt128 v -> show v
    WasmNullExternRef -> "Null"
    WasmNullFuncRef -> "Null"
    WasmExternRef hsref -> show hsref
    WasmFuncRef fcxt -> unsafePerformIO $ withFunctionInstanceContext fcxt (pure . show)

getValType :: WasmVal -> ValType
getValType v = unsafePerformIO $ withWasmVal v (fmap cToEnum . {#get WasmVal.Type #})

{-|
Generate the I32 WASM value.
-}
{#fun pure unsafe ValueGenI32 as ^ 
  {+
  , `Int32'         -- ^ the reference of WasmEdge_Value struct where the I32 value would be returned
  } -> `WasmVal'    -- ^ the I32 value. 
#}

{-|
  Retrieve the I32 value from the WASM value. 
-}
{#fun pure unsafe ValueGetI32 as ^
  { `WasmVal' -- ^the WASM value.
  } -> `Int32' -- ^ I32 value in the input
#}

{-|
  Generate the I64 WASM value.
-}
{#fun pure unsafe ValueGenI64 as ^ 
  {+
  , `Int64'           -- ^ the I64 value.
  } -> `WasmVal'      -- ^ WasmEdge_Value struct with the I64 value. 
#}

{-|
  Retrieve the I64 value from the WASM value.
-}
{#fun pure unsafe ValueGetI64 as ^ 
  {`WasmVal'          -- ^ the WasmEdge_Value struct.
  } -> `Int64'        -- ^ I64 value in the input struct. WASMEDGE_CAPI_EXPORT extern int64_t
#}

{-|
  Generate the F32 WASM value.
-}
{#fun pure unsafe ValueGenF32 as ^ 
  {+
  , `Float'         -- ^ the F32 value.
  } -> `WasmVal'    -- ^ WasmEdge_Value struct with the F32 value. 
#}

{-|
  Retrieve the F32 value from the WASM value.
-}
{#fun pure unsafe ValueGetF32 as ^ 
  {`WasmVal'      -- ^ the WasmEdge_Value struct.
  } -> `Float'    -- ^ F32 value in the input struct. 
#}

{-|
  Generate the F64 WASM value.
-}
{#fun pure unsafe ValueGenF64 as ^ 
  {+
  , `Double'        -- ^ the F64 value.
  } -> `WasmVal'    -- ^ WasmEdge_Value struct with the F64 value. 
#}

{-|
  Retrieve the F64 value from the WASM value.
-}
{#fun pure unsafe ValueGetF64 as ^ 
  {`WasmVal'          -- ^ the WasmEdge_Value struct.
  } -> `Double'       -- ^ F64 value in the input struct. 
#}

{-|
  Generate the V128 WASM value.
-}
{#fun pure unsafe ValueGenV128 as ^ 
  {+, 
  allocI128*`Int128'        -- ^ the V128 value.
  } -> `WasmVal'            -- ^ WasmEdge_Value struct with the V128 value.
#}

{-|
  Retrieve the V128 value from the WASM value.
-}
{#fun pure unsafe ValueGetV128 as ^ 
  {`WasmVal'                    -- ^ the WasmEdge_Value struct.
  , alloca-`Int128'peekI128*    -- ^ V128 value in the input struct.
  } -> `()' 
#}

{-|
  Generate the NULL reference WASM value.
-}
{#fun pure unsafe ValueGenNullRef as ^ 
  {+
  , cFromEnum`RefType'      -- ^ The reference type
  } -> `WasmVal'            -- ^ WasmEdge_Value struct with the NULL reference. 
#}

{-|
  Specify the WASM value is a null reference or not.
-}
{#fun pure unsafe ValueIsNullRef as ^ 
  {`WasmVal'    -- ^ the WasmEdge_Value struct.
  } -> `Bool'   -- ^ true if the value is a null reference, false if not. 
#}

{-|
Creation of the WasmEdge_String with the C string.
-}
{#fun pure unsafe StringCreateByCStringOut as stringCreateByCString 
  {allocWasmString-`WasmString'useCAndFinalizerFree*
  ,`String'           -- ^ the NULL-terminated C string to copy into the WasmEdge_String object.
  } -> `()'   -- ^ string object. Length will be 0 and Buf will be NULL if failed or the input string is a NULL. 
#}

{- |
Creation of the WasmEdge_String with the buffer and its length.
-}
{#fun pure unsafe StringCreateByBufferOut as stringCreateByBuffer
  {allocWasmString-`WasmString'useCAndFinalizerFree*
  , unsafeUseAsCStringLenBS*`ByteString'&  -- ^ the buffer to copy into the WasmEdge_String object.
  } -> `()'
#}

{- |
Create the WasmEdge_String wraps to the buffer.
-}
{#fun pure unsafe StringWrapOut as stringWrap
 {allocWasmString-`WasmString'useFinalizerFree*
 , useAsCStringLenBS*`ByteString'&
 } -> `()' #}

{-|
Compare the two WasmEdge_String objects.
-}
{#fun pure unsafe WasmEdge_StringIsEqual as wasmStringEq 
  {%`WasmString'        -- ^ the first WasmEdge_String object to compare.
  , %`WasmString'       -- ^ the second WasmEdge_String object to compare.
  } -> `Bool'           -- ^ true if the content of two WasmEdge_String objects are the same, false if not. 
#}

{-|
  Copy the content of WasmEdge_String object to the buffer.
  This function copy at most `Len` characters from the `WasmEdge_String` object to the destination buffer. If the string length is less than `Len` characters long, the remainder of the buffer is filled with `\0' characters. Otherwise, the destination is not terminated.
-}
{#fun unsafe WasmEdge_StringCopy as stringCopy_ 
  {%`WasmString'                  -- ^ the source WasmEdge_String object to copy.
  , id`Ptr CChar'            -- ^ the buffer to fill the string content.
  , `Word32'                      -- ^ the length of the buffer
} -> `Int' #}                  -- ^ the copied length of string.

{#fun pure unsafe C_Result_Success as mkResultSuccess {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Terminate as mkResultTerminate {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Fail as mkResultFail {+} -> `WasmResult' #}


class HasFinalizer t where
  runFinalizer :: t -> IO ()
  default runFinalizer :: Coercible t (ForeignPtr t) => t -> IO ()
  runFinalizer t = finalizeForeignPtr @t (coerce t)
  {-#INLINE runFinalizer #-}

  getFinalizer :: FinalizerPtr t

instance (TypeError ('Text "Use `withWasmResF` or `withWasmResT` instead of `withWasmRes`")) => HasFinalizer (Maybe t) where
  runFinalizer = error "Unreachable code"
  getFinalizer = error "Unreachable code"
 
{-|
  Finalize
-}
finalize :: HasFinalizer t => t -> IO ()
finalize = runFinalizer

{-|
  Copy the content of WasmEdge_String object to the buffer.
-}
stringCopy :: Word32 -> WasmString -> ByteString
stringCopy sz wstr = unsafePerformIO $ IntBS.createAndTrim (fromIntegral sz) $ \pw8 -> stringCopy_ wstr (castPtr @Word8 @CChar pw8) sz

instance Eq WasmString where
  (==) = wasmStringEq

instance IsString WasmString where
  fromString = stringCreateByBuffer . Char8.pack

instance Eq WasmResult where
  wr1 == wr2 = unsafePerformIO $ withWasmResult wr1 $ \wrp1 ->
    withWasmResult wr2 $ \wrp2 -> do
    r1 <- {#get WasmEdge_Result.Code #} wrp1
    r2 <- {#get WasmEdge_Result.Code #} wrp2
    pure $ r1 == r2

instance Show WasmResult where
  show = \case
    WRSuccess -> "Success"
    WRTerminate -> "Terminate"
    WRFail -> "Fail"
    wr@(WRError cat code) -> intercalate " " ["Error", show cat, show code, Char8.unpack $ resultGetMessage wr]
    
pattern WRSuccess :: WasmResult
pattern WRSuccess <- ((mkResultSuccess ==) -> True) where
  WRSuccess = mkResultSuccess

pattern WRTerminate :: WasmResult
pattern WRTerminate <- ((mkResultTerminate ==) -> True) where
  WRTerminate = mkResultTerminate

pattern WRFail :: WasmResult
pattern WRFail <- ((mkResultFail ==) -> True) where
  WRFail = mkResultFail

pattern WRError :: ErrCategory -> ErrCode -> WasmResult
pattern WRError errCat code <- ((resultOK &&& resultGetCategory &&& (toEnum . fromIntegral . resultGetCode)) -> (False, (errCat, code))) where
  WRError errCat code = resultGen errCat (fromIntegral $ fromEnum code)

{-# COMPLETE WRSuccess, WRTerminate, WRFail, WRError #-}  

{- |
  Returning Length of WasmEdge_String 
-}
wasmStringLength :: WasmString -> Word32
wasmStringLength wstr = unsafePerformIO $ withWasmString wstr (fmap fromIntegral . {#get WasmEdge_String.Length #})

{-|
  Converting WasmEdge_String to Text
-}
toText :: WasmString -> Text
toText wstr = unsafePerformIO $ withWasmString wstr $ \p -> do
  cstr <- {#get WasmEdge_String.Buf #} p
  cstrLen <- {#get WasmEdge_String.Length #} p
  T.peekCStringLen (cstr, fromIntegral cstrLen)

instance Show WasmString where
  show = T.unpack . toText

cToEnum :: Enum a => CInt -> a
cToEnum = toEnum . fromIntegral

cFromEnum :: Enum a => a -> CInt
cFromEnum = fromIntegral . fromEnum

{-|
Check the result is a success or not.
-}
{#fun pure unsafe ResultOK as resultOK 
  {%`WasmResult'              -- ^ the WasmEdge_Result struct.
  } -> `Bool'                 -- ^ true if the error code is WasmEdge_Result_Success or WasmEdge_Result_Terminate, false for others.
#}

{-|
  Generate the result with code.
-}
{#fun pure unsafe ResultGenOut as resultGen 
  {+
  , cFromEnum`ErrCategory'      -- ^ the WasmEdge_ErrCategory to specify the error category.
  , `CUInt'                     -- ^ the 24-bit length error code. The data exceeds 24 bits will be stripped.
  } -> `WasmResult'             -- ^ WasmEdge_Result struct with the given data. 
#}

{-|
  Get the result code.
 
  \param Res the WasmEdge_Result struct.
 
  \returns result code (24-bit size data) in the WasmEdge_Result struct.
-}
{#fun pure unsafe ResultGetCode as ^ {%`WasmResult'} -> `Word32' #}
{-|
  Get the error category.
-}
{#fun pure unsafe ResultGetCategory as ^ 
  {%`WasmResult'                  -- ^ the WasmEdge_Result struct.
  } -> `ErrCategory'cToEnum       -- ^ error category in the WasmEdge_Result struct. 
#}

{-|
  Get the result message.
  The returned string must __NOT__ be destroyed. If the error category of the result is __NOT__ `WasmEdge_ErrCategory_WASM`, the message will always be "user defined error code".
-}
{#fun pure unsafe ResultGetMessage as ^ 
  {%`WasmResult'                      -- ^ the WasmEdge_Result struct.
  } -> `ByteString'packCStringBS*     -- ^ NULL-terminated C string of the corresponding error message. 
#}
{#fun pure unsafe WasmEdge_LimitIsEqual as limitEq_ {%`Limit',%`Limit'} -> `Bool'#}

instance Eq Limit where
  (==) = limitEq_

{-|
  Opaque struct of WasmEdge configure.
-}
{#pointer *ConfigureContext as ^ foreign finalizer ConfigureDelete as ^ newtype #}

instance HasFinalizer ConfigureContext where
  getFinalizer = configureDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *StatisticsContext as ^ foreign finalizer StatisticsDelete as ^ newtype #}

instance HasFinalizer StatisticsContext where
  getFinalizer = statisticsDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ASTModuleContext as ^ foreign finalizer ASTModuleDelete as ^ newtype #}

instance HasFinalizer ASTModuleContext where
  getFinalizer = aSTModuleDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *FunctionTypeContext as ^ foreign finalizer FunctionTypeDelete as ^ newtype #}

instance HasFinalizer FunctionTypeContext where
  getFinalizer = functionTypeDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *MemoryTypeContext as ^ foreign finalizer MemoryTypeDelete as ^ newtype #}

instance HasFinalizer MemoryTypeContext where
  getFinalizer = memoryTypeDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *TableTypeContext as ^ foreign finalizer TableTypeDelete as ^ newtype #}

instance HasFinalizer TableTypeContext where
  getFinalizer = tableTypeDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *GlobalTypeContext as ^ foreign finalizer GlobalTypeDelete as ^ newtype #}

instance HasFinalizer GlobalTypeContext where
  getFinalizer = globalTypeDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ImportTypeContext as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ExportTypeContext as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *CompilerContext as ^ foreign finalizer CompilerDelete as ^ newtype #}

instance HasFinalizer CompilerContext where
  getFinalizer = compilerDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *LoaderContext as ^ foreign finalizer LoaderDelete as ^ newtype #}

instance HasFinalizer LoaderContext where
  getFinalizer = loaderDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ValidatorContext as ^ foreign finalizer ValidatorDelete as ^ newtype #}

instance HasFinalizer ValidatorContext where
  getFinalizer = validatorDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ExecutorContext as ^ foreign finalizer ExecutorDelete as ^ newtype #}

instance HasFinalizer ExecutorContext where
  getFinalizer = executorDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *StoreContext as ^ foreign finalizer StoreDelete as ^ newtype #}

instance HasFinalizer StoreContext where
  getFinalizer = storeDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ModuleInstanceContext as ^ foreign finalizer ModuleInstanceDelete as ^ newtype #}

instance HasFinalizer ModuleInstanceContext where
  getFinalizer = moduleInstanceDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *FunctionInstanceContext as ^ foreign finalizer FunctionInstanceDelete as ^ newtype #}

instance HasFinalizer FunctionInstanceContext where
  getFinalizer = functionInstanceDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *TableInstanceContext as ^ foreign finalizer TableInstanceDelete as ^ newtype #}

instance HasFinalizer TableInstanceContext where
  getFinalizer = tableInstanceDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *MemoryInstanceContext as ^ foreign finalizer MemoryInstanceDelete as ^ newtype #}

instance HasFinalizer MemoryInstanceContext where
  getFinalizer = memoryInstanceDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *GlobalInstanceContext as ^ foreign finalizer GlobalInstanceDelete as ^ newtype #}

instance HasFinalizer GlobalInstanceContext where
  getFinalizer = globalInstanceDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *CallingFrameContext as ^ foreign newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *Async as ^ foreign finalizer AsyncFinalize as ^ newtype #}

instance HasFinalizer Async where
  getFinalizer = asyncFinalize

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *VMContext as ^ foreign finalizer VMDelete as ^ newtype #}

instance HasFinalizer VMContext where
  getFinalizer = vMDelete

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *PluginContext as ^ foreign newtype #}

{-|
  Generate the function reference WASM value.
  The values generated by this function are only meaningful when the `WasmEdge_Proposal_BulkMemoryOperations` or the `WasmEdge_Proposal_ReferenceTypes` turns on in configuration.
-}
{#fun pure unsafe ValueGenFuncRef as ^ 
  {+
  , `FunctionInstanceContext'         -- ^ the function instance context to convert to the reference.
  } -> `WasmVal'                      -- ^ WasmEdge_Value struct with the function reference. 
#}

{-|
  Retrieve the function instance context from the WASM value.
-}
{#fun pure unsafe ValueGetFuncRef as ^ 
  {`WasmVal'                        -- ^ the WasmEdge_Value struct.
  } -> `FunctionInstanceContext'    -- ^ pointer to function instance context in the input struct.
#}

{-|
  Generate the function reference WASM value.
  The values generated by this function are only meaningful when the `WasmEdge_Proposal_ReferenceTypes` turns on in configuration.
-}
{#fun pure unsafe ValueGenExternRef as ^ 
  {+
  , fromHsRefIn*`HsRef'       -- ^ the reference to the external object.
  } -> `WasmVal'              -- ^ WasmEdge_Value struct with the external reference. 
#}

{-|
  Retrieve the external reference from the WASM value.
-}
{#fun pure unsafe ValueGetExternRef as ^ 
  {`WasmVal'                    -- ^ the WasmEdge_Value struct.
  } -> `HsRef'toHsRefOut*       -- ^ external reference in the input struct. 
#}

deriving newtype instance Storable ImportTypeContext
deriving newtype instance Storable ExportTypeContext

{-|
Type of option value.
-}
{#enum ProgramOptionType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WasmEdge logging functions
{-|
  Set the logging system to filter to error level.
-}
{#fun unsafe LogSetErrorLevel as ^ {} -> `()'#}

{-|
  Set the logging system to filter to debug level.
-}
{#fun unsafe LogSetDebugLevel as ^ {} -> `()'#}

{-|
  Set the logging system off.
-}
{#fun unsafe LogOff as ^ {} -> `()'#}

-- | WASM Proposal C enumeration.
{#enum Proposal as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum Proposal instance Storable Proposal

-- | Host Module Registration C enumeration.
{#enum HostRegistration as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum HostRegistration instance Storable HostRegistration

-- | AOT compiler optimization level C enumeration.
{#enum CompilerOptimizationLevel as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum CompilerOptimizationLevel instance Storable CompilerOptimizationLevel

-- | AOT compiler output binary format C enumeration.
{#enum CompilerOutputFormat as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum CompilerOutputFormat instance Storable CompilerOutputFormat

-- | Error category C enumeration.
{#enum ErrCategory as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum ErrCategory instance Storable ErrCategory

-- | Error code C enumeration.
{#enum ErrCode as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum ErrCode instance Storable ErrCode

-- | WASM Value type C enumeration.
{#enum ValType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq)#}
deriving via ViaFromEnum ValType instance Storable ValType

-- | WASM Number type C enumeration.
{#enum NumType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum NumType instance Storable NumType

-- | WASM Reference type C enumeration.
{#enum RefType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum RefType instance Storable RefType

-- | WASM Mutability C enumeration.
{#enum Mutability as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum Mutability instance Storable Mutability

-- | WASM External type C enumeration.
{#enum ExternalType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
deriving via ViaFromEnum ExternalType instance Storable ExternalType

-- Configure
{-|
  Creation of the WasmEdge_ConfigureContext.
-}
{#fun unsafe ConfigureCreate as ^ 
  {} -> `Maybe ConfigureContext'nullableFinalizablePtrOut*    -- pointer to the context, NULL if failed.
#}

{-|
  Add a proposal setting into the WasmEdge_ConfigureContext.
  This function is thread-safe.
-}
{#fun unsafe ConfigureAddProposal as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to add the proposal value.
  ,`Proposal'             -- ^ the proposal value.
  } -> `()'
#}

{-|
  Remove a proposal setting in the WasmEdge_ConfigureContext.
  This function is thread-safe.
-}
{#fun unsafe ConfigureRemoveProposal as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to remove the proposal.
  ,`Proposal'             -- ^ the proposal value.
  } -> `()'
#}

{-|
  Check if a proposal setting exists in the WasmEdge_ConfigureContext or not.
  This function is thread-safe.
-}
{#fun unsafe ConfigureHasProposal as ^ 
  {`ConfigureContext'    -- ^ the WasmEdge_ConfigureContext to check the proposal value.
  ,`Proposal'            -- ^ the proposal value.
  } -> `Bool'            -- ^ true if the proposal setting exists, false if not.
#}

{- |
  Add a built-in host registration setting into WasmEdge_ConfigureContext.
  For turning on the Wasi support in `WasmEdge_VMContext`, you can set the built-in host registration value into the `WasmEdge_ConfigureContext` and create VM with this context.
  ```c
  WasmEdge_ConfigureContext *Conf = WasmEdge_ConfigureCreate();
  WasmEdge_ConfigureAddHostRegistration(Conf, WasmEdge_HostRegistration_Wasi);
  WasmEdge_VMContext *VM = WasmEdge_VMCreate(Conf, NULL);
  ```
  This function is thread-safe.
-}
{#fun unsafe ConfigureAddHostRegistration as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to add built-in host registration.
  ,`HostRegistration'     -- ^ the built-in host registration value.
  } -> `()'
#}

{- |
  Remove a built-in host registration setting in the WasmEdge_ConfigureContext.
  This function is thread-safe.
-}
{#fun unsafe ConfigureRemoveHostRegistration as ^ 
  {`ConfigureContext'   -- ^ the WasmEdge_ConfigureContext to remove the host pre-registration.
  ,`HostRegistration'   -- ^ the built-in host registration value.
  } -> `()'
#}

{-|
  Check if a built-in host registration setting exists in the WasmEdge_ConfigureContext or not.
  This function is thread-safe.
-}
{#fun unsafe ConfigureHasHostRegistration as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to check the host pre-registration.
  ,`HostRegistration'     -- ^ the built-in host registration value.
  } -> `Bool'             -- ^ true if the built-in host registration setting exists, false if not.
#}

{-|
  Set the page limit of memory instances.
  Limit the page count (64KiB per page) in memory instances.
  This function is thread-safe.
-}
{#fun unsafe ConfigureSetMaxMemoryPage as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to set the maximum page count.
  , `Word32'              -- ^ the maximum page count.
  } -> `()'
#}

{-|
  Get the setting of the page limit of memory instances.
  This function is thread-safe.
-}
{#fun unsafe ConfigureGetMaxMemoryPage as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to get the maximum page count setting.
  } -> `Word32'           -- ^ the page count limitation value.
#}

{-|
  Set the force interpreter mode execution option.
  This function is thread-safe.
-}
{#fun unsafe ConfigureSetForceInterpreter as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to set the boolean value.
  , `Bool'                -- ^ the boolean value to determine to forcibly run WASM in interpreter mode or not.
  } -> `()'
#}

{-|
  Get the force interpreter mode execution option.
  This function is thread-safe.
-}
{#fun unsafe ConfigureIsForceInterpreter as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to get the boolean value.
  } -> `Bool'             -- ^ the boolean value to determine to forcibly run WASM in interpreter mode or not.
#}

{-|
  Set the optimization level of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerSetOptimizationLevel as ^ 
  {`ConfigureContext'               -- ^ the WasmEdge_ConfigureContext to set the optimization level.
  , `CompilerOptimizationLevel'     -- ^ the AOT compiler optimization level.
  } -> `()'
#}

{-|
  Get the optimization level of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerGetOptimizationLevel as ^ 
  {`ConfigureContext'               -- ^ the WasmEdge_ConfigureContext to get the optimization level.
  }-> `CompilerOptimizationLevel'   -- ^ the AOT compiler optimization level.
#}

{-|
  Set the output binary format of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerSetOutputFormat as ^ 
  {`ConfigureContext'         -- ^ the WasmEdge_ConfigureContext to set the output binary format.
  , `CompilerOutputFormat'    -- ^ the AOT compiler output binary format.
  } -> `()'
#}

{-|
  Get the output binary format of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerGetOutputFormat as ^ 
  {`ConfigureContext'             -- ^ the WasmEdge_ConfigureContext to get the output binary format.
  } -> `CompilerOutputFormat'     -- ^ the AOT compiler output binary format.
#}

{-|
  Set the dump IR option of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerSetDumpIR as ^ 
  {`ConfigureContext'           -- ^ the WasmEdge_ConfigureContext to set the boolean value.
  , `Bool'                      -- ^ the boolean value to determine to dump IR or not when compilation in AOT compiler.
  } -> `()'
#}

{-|
  Get the dump IR option of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerIsDumpIR as ^ 
  {`ConfigureContext'           -- ^ the WasmEdge_ConfigureContext to get the boolean value.
  } -> `Bool'                   -- ^ the boolean value to determine to dump IR or not when compilation in AOT compiler.
#}

{-|
  Set the generic binary option of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerSetGenericBinary as ^ 
  {`ConfigureContext'           -- ^ the WasmEdge_ConfigureContext to set the boolean value.
  , `Bool'                      -- ^ the boolean value to determine to generate the generic binary or not when compilation in AOT compiler.
  } -> `()'
#}

{-|
  Get the generic binary option of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerIsGenericBinary as ^ 
  {`ConfigureContext'            -- ^ the WasmEdge_ConfigureContext to get the boolean value.
  } -> `Bool'                    -- ^ the boolean value to determine to generate the generic binary or not when compilation in AOT compiler.
#}

{-|
  Set the interruptible option of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerSetInterruptible as ^ 
  {`ConfigureContext'           -- ^ the WasmEdge_ConfigureContext to set the boolean value.
  , `Bool'                      -- ^ the boolean value to determine to generate interruptible binary or not when compilation in AOT compiler.
  } -> `()'
#}

{-|
  Get the interruptible option of the AOT compiler.
  This function is thread-safe.
-}
{#fun unsafe ConfigureCompilerIsInterruptible as ^ 
  {`ConfigureContext'           -- ^ the WasmEdge_ConfigureContext to get the boolean value.
  } -> `Bool'                   -- ^ the boolean value to determine to generate interruptible binary or not when compilation in AOT compiler.
#}

{-|
  Set the instruction counting option for the statistics.
  This function is thread-safe.
-}
{#fun unsafe ConfigureStatisticsSetInstructionCounting as ^ 
  {`ConfigureContext'      -- ^ the WasmEdge_ConfigureContext to set the boolean value.
  , `Bool'                 -- ^ the boolean value to determine to support instruction counting when execution or not after compilation by the AOT compiler.
  } -> `()'
  #}

{-|
  Get the instruction counting option for the statistics.
  This function is thread-safe.
-}
{#fun unsafe ConfigureStatisticsIsInstructionCounting as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to get the boolean value.
  } -> `Bool'             -- ^ the boolean value to determine to support instruction counting when execution or not after compilation by the AOT compiler.
#}

{-|
  Set the cost measuring option for the statistics.
  This function is thread-safe.
-}
{#fun unsafe ConfigureStatisticsSetCostMeasuring as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to set the boolean value.
  , `Bool'                -- ^ the boolean value to determine to support cost measuring when execution or not after compilation by the AOT compiler.
  } -> `()'
#}

{-|
  Set the cost measuring option for the statistics.
  This function is thread-safe.
-}
{#fun unsafe ConfigureStatisticsIsCostMeasuring as ^ 
  {`ConfigureContext'   -- ^ the WasmEdge_ConfigureContext to set the boolean value.
  } -> `Bool'           -- ^ the boolean value to determine to support cost measuring when execution or not after compilation by the AOT compiler.
#}

{-|
  Get the cost measuring option for the statistics.
  This function is thread-safe.
-}
{#fun unsafe ConfigureStatisticsSetTimeMeasuring as ^ 
  {`ConfigureContext'     -- ^ the WasmEdge_ConfigureContext to get the boolean value.
  , `Bool'                -- ^ the boolean value to determine to support cost measuring when execution or not after compilation by the AOT compiler.
  } -> `()'
#}

{-|
  Set the time measuring option for the statistics.
  This function is thread-safe.
-}
{#fun unsafe ConfigureStatisticsIsTimeMeasuring as ^ 
  {`ConfigureContext'         -- ^ the WasmEdge_ConfigureContext to set the boolean value.
  } -> `Bool'                 -- ^ the boolean value to determine to support time when execution or not after compilation by the AOT compiler.
#}

-- Statistics
{-|
  Creation of the WasmEdge_StatisticsContext.
  The caller owns the object and should call `WasmEdge_StatisticsDelete` to destroy it.
-}
{#fun unsafe StatisticsCreate as ^ 
{} -> `Maybe StatisticsContext'nullableFinalizablePtrOut*   -- ^ pointer to context, NULL if failed. 
#}

{-|
  Get the instruction count in execution.
-}
{#fun unsafe StatisticsGetInstrCount as ^ 
  {`StatisticsContext'     -- ^ the WasmEdge_StatisticsContext to get data.
  } -> `Word64'            -- ^ the instruction count in total execution.
#}

{-|
  Get the instruction count per second in execution.
-}
{#fun unsafe StatisticsGetInstrPerSecond as ^ 
  {`StatisticsContext'    -- ^ the WasmEdge_StatisticsContext to get data.
  } -> `Double'           -- ^ the instruction count per second.
#}

{-|
  Get the total cost in execution.
-}
{#fun unsafe StatisticsGetTotalCost as ^ 
  {`StatisticsContext'    -- ^ the WasmEdge_StatisticsContext to get data.
  } -> `Word64'           -- ^ the total cost.
#}

{-|
  Set the costs of instructions.
-}
{#fun unsafe StatisticsSetCostTable as ^ 
  {`StatisticsContext'                    -- ^ the WasmEdge_StatisticsContext to set the cost table.
  , fromStoreVecOr0Ptr*`Vector Word64'&   -- ^ the cost table array and the length of the cost table array.
  } -> `()'
#}

{-|
  Set the cost limit in execution.
  The WASM execution will be aborted if the instruction costs exceeded the limit and the ErrCode::Value::CostLimitExceeded will be returned.
-}
{#fun unsafe StatisticsSetCostLimit as ^ 
  {`StatisticsContext'      -- ^ the WasmEdge_StatisticsContext to set the cost table.
  , `Word64'                -- ^ the cost limit.
  } -> `()'
#}

{-|
  Clear all data in the WasmEdge_StatisticsContext.
-}
{#fun unsafe StatisticsClear as ^ 
  {`StatisticsContext'    -- ^ the WasmEdge_StatisticsContext to clear.
  } -> `()'
#}

-- AST Module
{-|
  Get the length of imports list of the AST module.
-}
{#fun unsafe ASTModuleListImportsLength as astModuleListImportsLength 
  {`ASTModuleContext'          -- ^ the WasmEdge_ASTModuleContext.
  } -> `Word32'                -- ^ length of the imports list.
#}
{#fun unsafe ASTModuleListImports as astModuleListImports_ {`ASTModuleContext', fromMutIOVecOr0Ptr*`IOVector (ImportTypeContext)'&} -> `Word32'#}

{-|
  Get the length of exports list of the AST module.
-}
{#fun unsafe ASTModuleListExportsLength as astModuleListExportsLength 
  {`ASTModuleContext'         -- ^ the WasmEdge_ASTModuleContext.
  } -> `Word32'               -- ^ length of the exports list.
#}
-- TODO:
-- why is there the _ ?
{#fun unsafe ASTModuleListExports as astModuleListExports_ {`ASTModuleContext', fromMutIOVecOr0Ptr*`IOVector ExportTypeContext'&} -> `Word32'#}

-- Function
{-|
  Creation of the WasmEdge_FunctionTypeContext.
-}
{#fun unsafe FunctionTypeCreate as ^ 
  {fromStoreVecOr0Ptr*`Vector ValType'&,    -- ^ the value types list of parameters. NULL if the length is 0 and the buffer length 
  fromStoreVecOr0Ptr*`Vector ValType'&      -- ^ the value types list of returns. NULL if the length is 0 and the buffer length
  } -> `Maybe FunctionTypeContext'nullableFinalizablePtrOut* -- ^ pointer to context, NULL if failed.
#}

{-|
  Get the parameter types list length from the WasmEdge_FunctionTypeContext.
-}
{#fun unsafe FunctionTypeGetParametersLength as ^ 
  {`FunctionTypeContext'        -- ^ the WasmEdge_FunctionTypeContext.
  } -> `Word32'                 -- ^ the parameter types list length.
#}
{#fun unsafe FunctionTypeGetParameters as functionTypeGetParameters_ {`FunctionTypeContext', fromMutIOVecOfCEnumOr0Ptr*`IOVector ValType'&} -> `Word32'#}


{-|
Get the parameter types list from the WasmEdge_FunctionTypeContext.
-}
functionTypeGetParameters :: 
  FunctionTypeContext           -- ^ the WasmEdge_FunctionTypeContext. 
  -> Word32                     -- ^ [out] List the WasmEdge_ValType buffer to fill the parameter value types.
  -> IO (Vector ValType)
functionTypeGetParameters fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- functionTypeGetParameters_ fcxt v
  VS.unsafeFreeze $ VSM.slice 0 (fromIntegral len) v

{-|
List the imports of the AST module.
-}
astModuleListImports ::
  ASTModuleContext -- ^ the WasmEdge_ASTModuleContext.
  -> Word32 -- ^ the buffer length.
  -> IO (Vector ImportTypeContext) -- ^ the import type contexts
astModuleListImports fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- astModuleListImports_ fcxt v
  VS.unsafeFreeze $ VSM.unsafeCoerceMVector $ VSM.slice 0 (fromIntegral len) v

astModuleListExports :: ASTModuleContext -> Word32 -> IO (Vector ExportTypeContext)
astModuleListExports fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- astModuleListExports_ fcxt v
  VS.unsafeFreeze $ VSM.unsafeCoerceMVector $ VSM.slice 0 (fromIntegral len) v  
  
-- Function Type
{-|
Get the return types list length from the WasmEdge_FunctionTypeContext.
-}
{#fun unsafe FunctionTypeGetReturnsLength as ^ 
  {`FunctionTypeContext'    -- ^ the WasmEdge_FunctionTypeContext.
  } -> `Word32'             -- ^ the return types list length.
#}
{#fun unsafe FunctionTypeGetReturns as functionTypeGetReturns_ {`FunctionTypeContext', fromMutIOVecOfCEnumOr0Ptr*`IOVector ValType'&} -> `Word32'#}

{-|
Get the return types list from the WasmEdge_FunctionTypeContext.
-}
functionTypeGetReturns :: 
  FunctionTypeContext         -- ^ the WasmEdge_FunctionTypeContext. 
  -> Word32                  -- ^ [out] List the WasmEdge_ValType buffer to fill the return value and the length
  -> IO (Vector ValType)     -- ^ the actual return types list length.  
functionTypeGetReturns fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- functionTypeGetReturns_ fcxt v
  VS.unsafeFreeze $ VSM.slice 0 (fromIntegral len) v

-- Table Type
{-|
  Creation of the WasmEdge_TableTypeContext.
  The caller owns the object and should call `WasmEdge_TableTypeDelete` to destroy it.
-}
{#fun unsafe TableTypeCreate as ^ 
  {`RefType'                  -- ^ the reference type of the table type.
  ,%`Limit'                   -- ^ the limit struct of the table type.
  } -> `Maybe TableTypeContext'nullableFinalizablePtrOut* -- ^ pointer to context, NULL if failed.
#}

{-|
  Get the reference type from a table type.
-}
{#fun unsafe TableTypeGetRefType as ^ 
  {`TableTypeContext'         -- ^ the WasmEdge_TableTypeContext.
  } -> `RefType'              -- ^ the reference type of the table type.
#}      

{-|
  Get the limit from a table type.
-}
{#fun unsafe TableTypeGetLimitOut as tableTypeGetLimit 
  {+
  ,`TableTypeContext'         -- ^ the WasmEdge_TableTypeContext.
  } -> `Limit'                -- ^ the limit struct of the table type.
#}

-- Memory Type
{- |
  Creation of the WasmEdge_MemoryTypeContext.
  The caller owns the object and should call `WasmEdge_MemoryTypeDelete` to destroy it.
-}
{#fun unsafe MemoryTypeCreate as ^ 
  {%`Limit'                   -- ^ the limit struct of the memory type.
  } -> `Maybe MemoryTypeContext'nullableFinalizablePtrOut*  -- ^ pointer to context, NULL if failed.
#}  

{-|
  Get the limit from a memory type.
-}
{#fun unsafe MemoryTypeGetLimitOut as memoryTypeGetLimit 
  {+
  ,`MemoryTypeContext'      -- ^ the WasmEdge_MemoryTypeContext.
  } -> `Limit'              -- ^ the limit struct of the memory type.
#} 

-- Global Type
{-|
  Creation of the WasmEdge_GlobalTypeContext.
  The caller owns the object and should call `WasmEdge_GlobalTypeDelete` to destroy it.
-}
{#fun unsafe GlobalTypeCreate as ^ 
  {`ValType'                  -- ^ the value type of the global type.
  ,`Mutability'               -- ^ the mutation of the global type.
  } -> `Maybe GlobalTypeContext'nullableFinalizablePtrOut* -- ^ pointer to context, NULL if failed.
#} 

{-|
Get the value type from a global type.
-}
{#fun unsafe GlobalTypeGetValType as ^ 
  {`GlobalTypeContext'      -- ^ the WasmEdge_GlobalTypeContext.
  } -> `ValType'            -- ^ the value type of the global type.
#} 

{-|
  Get the mutability from a global type.
-}
{#fun unsafe GlobalTypeGetMutability as ^ 
  {`GlobalTypeContext'      -- ^ the WasmEdge_GlobalTypeContext.
  } -> `Mutability'         -- ^ the mutability of the global type.
#}

-- Import Type
{-|
  Get the module name from an import type.
-}
{#fun unsafe ImportTypeGetModuleNameOut as importTypeGetModuleName 
  {+
  ,`ImportTypeContext'            -- ^ the WasmEdge_ImportTypeContext.
  } -> `WasmString'               -- ^ string object. Length will be 0 and Buf will be NULL if failed.
#}

{- |
  Get the external name from an import type.
-}
{#fun unsafe ImportTypeGetExternalNameOut as importTypeGetExternalName 
  {+
  ,`ImportTypeContext'            -- ^ the WasmEdge_ImportTypeContext.
  } -> `WasmString'               -- ^ string object. Length will be 0 and Buf will be NULL if failed.
#}

{-|
  Get the external value (which is function type) from an import type.
-}
-- Question: Why have you written noFinalizer here
{#fun unsafe ImportTypeGetFunctionType as ^ 
  {`ASTModuleContext'                     -- ^ the WasmEdge_ASTModuleContext.
  ,`ImportTypeContext'                    -- ^ the WasmEdge_ImportTypeContext which queried from the `ASTCxt`.
  } -> `FunctionTypeContext'noFinalizer*  -- ^ the function type. NULL if failed or the external type of the import type is not `WasmEdge_ExternalType_Function`.
#} 

{-|
  Get the external value (which is table type) from an import type.
-}
{#fun unsafe ImportTypeGetTableType as ^ 
  {`ASTModuleContext'         -- ^ the WasmEdge_ASTModuleContext.
  ,`ImportTypeContext'        -- ^ the WasmEdge_ImportTypeContext which queried from the `ASTCxt`.
  } -> `TableTypeContext'     -- ^ the table type. NULL if failed or the external type of the import type is not `WasmEdge_ExternalType_Table`.
#} 

{-|
  Get the external value (which is memory type) from an import type.
-}
{#fun unsafe ImportTypeGetMemoryType as ^ 
  {`ASTModuleContext'         -- ^ the WasmEdge_ASTModuleContext.
  ,`ImportTypeContext'        -- ^ the WasmEdge_ImportTypeContext which queried from the `ASTCxt`.
  } -> `MemoryTypeContext'    -- ^ the memory type. NULL if failed or the external type of the import type is not `WasmEdge_ExternalType_Memory`.
#} 

{-|
  Get the external value (which is global type) from an import type.
-}
{#fun unsafe ImportTypeGetGlobalType as ^ 
  {`ASTModuleContext'     -- ^ the WasmEdge_ASTModuleContext.
  ,`ImportTypeContext'    -- ^ the WasmEdge_ImportTypeContext which queried from the `ASTCxt`.
  } -> `GlobalTypeContext' -- ^ the global type. NULL if failed or the external type of the import type is not `WasmEdge_ExternalType_Global`.
#} 

-- Export Type
{-|
  Get the external type from an export type.
-}
{#fun unsafe ExportTypeGetExternalType as ^ 
  {`ExportTypeContext'    -- ^ the WasmEdge_ExportTypeContext.
  } -> `ExternalType'     -- ^ the external type of the export type.
#} 

{-|
  Get the external name from an export type.
  The returned string object is linked to the external name of the export type, and the caller should __NOT__ call the `WasmEdge_StringDelete`.
-}
{#fun unsafe ExportTypeGetExternalNameOut as exportTypeGetExternalName 
  {+
  ,`ExportTypeContext'        -- ^ the WasmEdge_ExportTypeContext.
  } -> `WasmString'           -- ^ string object. Length will be 0 and Buf will be NULL if failed.
#} 

{-|
  Get the external value (which is function type) from an export type.
  The export type context should be the one queried from the AST module context, or this function will cause unexpected error.
-}
{#fun unsafe ExportTypeGetFunctionType as ^ 
  {`ASTModuleContext'         -- ^ the WasmEdge_ASTModuleContext.
  ,`ExportTypeContext'        -- ^ the WasmEdge_ExportTypeContext which queried from the `ASTCxt`.
  } -> `FunctionTypeContext'  -- ^ the function type. NULL if failed or the external type of the export type is not `WasmEdge_ExternalType_Function`.
#} 

{-|
  Get the external value (which is table type) from an export type.
-}
{#fun unsafe ExportTypeGetTableType as ^ 
  {`ASTModuleContext'         -- ^ the WasmEdge_ASTModuleContext.
  ,`ExportTypeContext'        -- ^ the WasmEdge_ExportTypeContext which queried from the `ASTCxt`.
  } -> `TableTypeContext'     -- ^ the table type. NULL if failed or the external type of the export type is not `WasmEdge_ExternalType_Table`.
#}

{-|
  Get the external value (which is memory type) from an export type.
-}
{#fun unsafe ExportTypeGetMemoryType as ^ 
  {`ASTModuleContext'         -- ^ the WasmEdge_ASTModuleContext.
  ,`ExportTypeContext'        -- ^ the WasmEdge_ExportTypeContext which queried from the `ASTCxt`.
  } -> `MemoryTypeContext'    -- ^ the memory type. NULL if failed or the external type of the export type is not `WasmEdge_ExternalType_Memory`.
#}

{-|
  Get the external value (which is global type) from an export type.
-}
{#fun unsafe ExportTypeGetGlobalType as ^ 
  {`ASTModuleContext'         -- ^ the WasmEdge_ASTModuleContext.
  ,`ExportTypeContext'        -- ^ the WasmEdge_ExportTypeContext which queried from the `ASTCxt`.
  } -> `GlobalTypeContext'    -- ^ the global type. NULL if failed or the external type of the export type is not `WasmEdge_ExternalType_Global`.
#}

-- AOT Compiler
{-|
  Creation of the WasmEdge_CompilerContext.
-}
{#fun unsafe CompilerCreate as ^ 
  {`ConfigureContext'       -- ^ the WasmEdge_CompilerContext.
  } -> `Maybe CompilerContext'nullableFinalizablePtrOut*  -- ^ pointer to context, NULL if failed.
#}

{- |
  Compile the input WASM from the file path.
  The compiler compiles the WASM from file path for the ahead-of-time mode and store the result to the output file path.
-}
{#fun unsafe CompilerCompileOut as compilerCompile 
  {+
  ,`CompilerContext'      -- ^ the WasmEdge_CompilerContext.
  ,`String'               -- ^ the input WASM file path.
  ,`String'               -- ^ the output WASM file path.
  } -> `WasmResult'       -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.         
#} 

{-|
  Compile the input WASM from the given buffer.
  The compiler compiles the WASM from the given buffer for the ahead-of-time mode and store the result to the output file path.
-}
{#fun unsafe CompilerCompileFromBufferOut as compilerCompileFromBuffer 
  {+
  ,`CompilerContext'                 -- ^ the WasmEdge_CompilerContext.
  , fromByteStringIn*`ByteString'&   -- ^ the input WASM binary buffer and the length of the binary buffer
  ,`String'                          -- ^ the output WASM file path.
  } -> `WasmResult'                  -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#} 

-- Loader
{-|
  Creation of the WasmEdge_LoaderContext.
  The caller owns the object and should call `WasmEdge_LoaderDelete` to destroy it.
-}
{#fun unsafe LoaderCreate as ^ 
  {`ConfigureContext'             -- ^ the WasmEdge_ConfigureContext as the configuration of Loader. NULL for the default configuration.
  } -> `Maybe LoaderContext'nullableFinalizablePtrOut*  -- ^ pointer to context, NULL if failed.
#}

{-|
  Load and parse the WASM module from the file path, and return a `ASTModuleContext` as the result.
  The caller owns the object.
-}
{#fun unsafe LoaderParseFromFileOut as loaderParseFromFile
   {+
   ,`LoaderContext'  -- ^ the WasmEdge `LoaderContext`.
   ,alloca-`Maybe ASTModuleContext'peekOutNullablePtr*
   ,`String'         -- ^ the WASM file path.
   } -> `WasmResult' -- ^ the result status & the output ASTModuleContext if succeeded.
#}

{-|
  Load and parse the WASM module from a buffer, and return a `ASTModuleContext` as the result.
  The caller owns the object.
-}
{#fun unsafe LoaderParseFromBufferOut as loaderParseFromBuffer
 {+
 , `LoaderContext'  -- ^ the WasmEdge `LoaderContext`.
 ,alloca-`Maybe ASTModuleContext'peekOutNullablePtr*
 ,useAsPtrCUCharLenBS*`ByteString'& -- ^ the buffer of WASM binary.
 } -> `WasmResult'#} -- ^ the result status & the output ASTModuleContext if succeeded.

-- Validator
{-|
  Creation of the WasmEdge_ValidatorContext.
  The caller owns the object and should call `WasmEdge_ValidatorDelete` to destroy it.
-}
{#fun unsafe ValidatorCreate as ^ 
  {`ConfigureContext'       -- ^ the WasmEdge_ConfigureContext as the configuration of Validator. NULL for the default configuration.
  } -> `Maybe ValidatorContext'nullableFinalizablePtrOut*   -- ^ pointer to context, NULL if failed.
#}

{-|
  Validate the WasmEdge AST Module.
-}
{#fun unsafe ValidatorValidateOut as validatorValidate 
  {+
  ,`ValidatorContext'       -- ^ the WasmEdge_ValidatorContext.
  ,`ASTModuleContext'       -- ^ the WasmEdge_ASTModuleContext to validate.
  } -> `WasmResult'         -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

-- Executor
{-|
  Creation of the WasmEdge_ExecutorContext.
  The caller owns the object and should call `finalize` to delete it.
-}
{#fun unsafe ExecutorCreate as ^ 
  {nullablePtrIn*`Maybe ConfigureContext'       -- ^ the WasmEdge_ConfigureContext as the configuration of Executor. NULL for the default configuration.
  ,nullablePtrIn*`Maybe StatisticsContext'      -- ^ the WasmEdge_StatisticsContext as the statistics object set into Executor. The statistics will refer to this context, and the life cycle should be guaranteed until the executor context is deleted. NULL for not doing the statistics.
  } -> `Maybe ExecutorContext'nullableFinalizablePtrOut*    -- ^ pointer to context, NULL if failed.
#}

{-|
  Instantiate an AST Module into a module instance.
  Instantiate an AST Module, and return an instantiated module instance context as the result. The caller owns the object and should call `WasmEdge_ModuleInstanceDelete` to destroy it. 
  Developers can use the `WasmEdge_ModuleInstanceListFunction`, `WasmEdge_ModuleInstanceFindFunction`, etc. APIs to retrieve the exported instances from the result module instance.
-}
{#fun unsafe ExecutorInstantiateOut as executorInstantiate 
  {+
  ,`ExecutorContext'                            -- ^ the WasmEdge_ExecutorContext to instantiate the module.
  ,alloca-`ModuleInstanceContext'peekOutPtr*    -- ^ [out] ModuleCxt the output WasmEdge_ModuleInstanceContext if succeeded
  ,`StoreContext'                               -- ^ the WasmEdge_StoreContext to link the imports.
  ,`ASTModuleContext'                           -- ^ the WasmEdge AST Module context generated by loader or compiler.
  } -> `WasmResult'                             -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Instantiate an AST Module into a named module instance and link into store.
-}
{#fun unsafe ExecutorRegisterOut as executorRegister 
  {+
  ,`ExecutorContext'                          -- ^ the WasmEdge_ExecutorContext to instantiate the module.
  ,alloca-`ModuleInstanceContext'peekOutPtr*  -- ^ [out] ModuleCxt the output WasmEdge_ModuleInstanceContext if succeeded.
  ,`StoreContext'                             -- ^ the WasmEdge_StoreContext to link the imports.
  ,`ASTModuleContext'                         -- ^ the WasmEdge AST Module context generated by loader or compiler.
  ,%`WasmString'                              -- ^ the module name WasmEdge_String for all exported instances.
  } -> `WasmResult'                           -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Register a module instance into a store with exporting its module name.
-}
{#fun unsafe ExecutorRegisterImportOut as executorRegisterImport 
  {+
  ,`ExecutorContext'        -- ^ the WasmEdge_ExecutorContext to instantiate the module.
  ,`StoreContext'           -- ^ the WasmEdge_StoreContext to store the instantiated module.
  ,`ModuleInstanceContext'  -- ^ the WasmEdge_ModulenstanceContext to register.
  } -> `WasmResult'         -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Invoke a WASM function by the function instance.
-}
executorInvoke ::
  ExecutorContext -- ^ the WasmEdge_ExecutorContext.
  -> FunctionInstanceContext -- ^ the function instance context to invoke.
  -> V.Vector WasmVal -- ^ the WasmEdge_Value buffer with the parameter values.
  -> IO (WasmResult, V.Vector WasmVal) -- ^ the WasmEdge_Value buffer to fill the return values.
executorInvoke ecxt ficxt pars = do
  funcTypeMay <- functionInstanceGetFunctionType ficxt
  case funcTypeMay of
    Nothing -> pure (WRFail, V.empty)
    Just funcType -> do 
      parLen <- fromIntegral <$> functionTypeGetParametersLength funcType
      retLen <- fromIntegral <$> functionTypeGetReturnsLength funcType
      if parLen /= V.length pars
        then pure (WRFail, V.empty)
        else do
        retsVSM <- VSM.generateM (retLen) (const $ allocWasmVal pure)
        wres <- executorInvoke_ ecxt ficxt pars retsVSM
        rets <- V.generateM (retLen) ((useFinalizerFree =<<) . (VSM.read retsVSM))
        pure (wres, rets)

{#fun unsafe ExecutorInvokeOut as executorInvoke_ 
  {+,`ExecutorContext'
  ,`FunctionInstanceContext'
  ,fromVecOfFPtr*`V.Vector WasmVal'&
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&
  } -> `WasmResult'
#}

{-|
  Asynchronous invoke a WASM function by the function instance.
  After instantiating a WASM module, developers can get the function instance context from the module instance. Then developers can invoke the function asynchronously through this API.
-}
{#fun unsafe ExecutorAsyncInvokeOut as executorAsyncInvoke 
  {`ExecutorContext'                              -- ^ the WasmEdge_ExecutorContext.
  ,`FunctionInstanceContext'                      -- ^ the function instance context to invoke.
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&   -- ^ the WasmEdge_Value buffer with the parameter values and the parameter length
  } -> `Async'                                    -- ^ WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call `WasmEdge_AsyncDelete` to destroy this object.
#}

-- Store
{-|
  Creation of the WasmEdge_StoreContext.
  The caller owns the object and should call `WasmEdge_StoreDelete` to destroy it.
-}
{#fun unsafe StoreCreate as ^ 
  {} -> `Maybe StoreContext'nullableFinalizablePtrOut*  -- ^ pointer to context, NULL if failed.
#} 

{-|
  Get the module instance context by the module name.
  After registering a WASM module, developers can call this function to find and get the registered module instance context by the module name.
  This function is thread-safe.
-}
{#fun unsafe StoreFindModule as ^ 
  {`StoreContext'                 -- ^ the WasmEdge_StoreContext.
  ,%`WasmString'                  -- ^ the module name WasmEdge_String.
  } -> `Maybe ModuleInstanceContext'nullableNoFinalizer*    -- ^ pointer to the module instance context. NULL if not found.
#}

{-|
  Get the length of registered module list in store.
  This function is thread-safe.
-}
{#fun unsafe StoreListModuleLength as ^ 
  {`StoreContext'         -- ^ the WasmEdge_StoreContext.
  } -> `Word32'           -- ^ length of registered named module list.
#}

{-|
  List the registered module names.
  If the `Names` buffer length is smaller than the result of the registered
  named module list size, the overflowed return values will be discarded.
  This function is thread-safe.
-}
{#fun unsafe StoreListModuleOut as storeListModule_ 
  {`StoreContext'                                   -- ^ the WasmEdge_StoreContext.
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&  -- ^ [out] Names the output names WasmEdge_String buffer of named modules and length of the buffer
  } -> `Word32'                                     -- ^ actual registered named module list size.
#}

{-|
  List the registered module names.
  If the `Names` buffer length is smaller than the result of the registered
  named module list size, the overflowed return values will be discarded.
  This function is thread-safe.
-}
storeListModule ::
  StoreContext -- ^ the WasmEdge_StoreContext.
  -> Word32 -- ^ the length of the buffer 
  -> IO (V.Vector WasmString) -- ^ the output names `WasmString` buffer of named modules
storeListModule store retLen = do
  retOut <- VSM.generateM (fromIntegral retLen) (const $ allocWasmString pure)
  res <- storeListModule_ store retOut
  rets <- V.generateM (fromIntegral res) ((useFinalizerFree =<<) . (VSM.read retOut))
  pure rets

-- Module Instance
{-|
  Creation of the WasmEdge_ModuleInstanceContext.
-}
{#fun unsafe ModuleInstanceCreate as ^ 
  {%`WasmString'                -- ^ the module name WasmEdge_String of this host module to import.
  } -> `Maybe ModuleInstanceContext'nullableFinalizablePtrOut*  -- ^ pointer to context, NULL if failed.
#} 

{-|
  Creation of the WasmEdge_ModuleInstanceContext with host data.
-}
{#fun ModuleInstanceCreateWithData as ^ 
  {%`WasmString'                      -- ^ the module name WasmEdge_String of this host module to import.
  ,fromHsRefWithFinalzrIn*`HsRef'&    -- ^ the host data to set into the module instance. When calling the finalizer, this pointer will become the argument of the finalizer function and Finalizer the function to finalize the host data.
  } -> `Maybe ModuleInstanceContext'nullableFinalizablePtrOut* -- ^ pointer to context, NULL if failed.
#}

{-|
  Creation of the WasmEdge_ModuleInstanceContext for the WASI specification.
  This function will create a WASI host module that contains the WASI host functions and initialize it. The caller owns the object and should call `WasmEdge_ModuleInstanceDelete` to destroy it.
-}
{#fun unsafe ModuleInstanceCreateWASI as ^ 
  {fromVecStringOr0Ptr*`V.Vector String'&  -- ^ the command line arguments. The first argument suggests being the program name. NULL if the length is 0. and the length
  ,fromVecStringOr0Ptr*`V.Vector String'&  -- ^ the environment variables in the format `ENV=VALUE`. NULL if the length is 0. and the length
  ,fromVecStringOr0Ptr*`V.Vector String'&  -- ^ the directory paths to preopen. String format in `PATH1:PATH2` means the path mapping, or the same path will be mapped. NULL if the length is 0. and the length of the paths
  } -> `Maybe ModuleInstanceContext'nullableFinalizablePtrOut*             -- ^ pointer to context, NULL if failed.
#}

{-|
  Initialize the WasmEdge_ModuleInstanceContext for the WASI specification.
  This function will initialize the WASI host module with the parameters.
-}
{#fun unsafe ModuleInstanceInitWASI as ^ 
  {`ModuleInstanceContext'                  -- ^ the WasmEdge_ModuleInstanceContext of WASI import object.
  ,fromVecStringOr0Ptr*`V.Vector String'&   -- ^ the command line arguments. The first argument suggests being the program name. NULL if the length is 0. and the length
  ,fromVecStringOr0Ptr*`V.Vector String'&   -- ^ the environment variables in the format `ENV=VALUE`. NULL if the length is 0. and the length
  ,fromVecStringOr0Ptr*`V.Vector String'&   -- ^ the directory paths to preopen. String format in `PATH1:PATH2` means the path mapping, or the same path will be mapped. NULL if the length is 0. and the length of the paths
} -> `()'#}

{-|
  Get the WASI exit code.
  This function will return the exit code after running the "_start" function of a `wasm32-wasi` program.
-}
{#fun unsafe ModuleInstanceWASIGetExitCode as ^ 
  {`ModuleInstanceContext'        -- ^ the WasmEdge_ModuleInstanceContext of WASI import object.
  } -> `Word32'                   -- ^ the exit code after executing the "_start" function. Return `EXIT_FAILURE` if the `Cxt` is NULL or not a WASI host module.
#}

{-|
  Get the native handler from the WASI mapped FD/Handler.
-}
{#fun unsafe ModuleInstanceWASIGetNativeHandler as ^ 
  {`ModuleInstanceContext'                  -- ^ the WasmEdge_ModuleInstanceContext of WASI import object.
  ,`Word32'                                 -- ^ the WASI mapped Fd.
  ,alloca-`Word64'peekCoerce*               -- ^ [out] NativeHandler the raw Fd/Handler.
  } -> `Word32'                             -- ^ the error code. Return `0` if the Native Handler is found. Return `1` if the `Cxt` is `NULL`. Return `2` if the given mapped Fd/handler is not found.
#}

{-|
  Initialize the WasmEdge_ModuleInstanceContext for the wasmedge_process specification.
  This function will initialize the wasmedge_process host module with the parameters.
-}
{#fun unsafe ModuleInstanceInitWasmEdgeProcess as ^ 
  {fromVecStringOr0Ptr*`V.Vector String'&     -- ^ the allowed commands white list. NULL if the length is 0. and length of the list
  ,`Bool'                                     -- ^ the boolean value to allow all commands. `false` is suggested. If this value is `true`, the allowed commands white list will not be recorded and all commands can be executed by wasmedge_process.
  } -> `()'
#}

{-|
  Get the export module name of a module instance.
-}
{#fun unsafe ModuleInstanceGetModuleNameOut as moduleInstanceGetModuleName 
  {+
  ,`ModuleInstanceContext'          -- ^ the WasmEdge_ModuleInstanceContext.
  } -> `WasmString'                 -- ^ string object. Length will be 0 and Buf will be NULL if failed.
#}

{-|
  Get the host data set into the module instance when creating.
  The returned data is owned by the module instance, and will be passed into the finalizer when deleting this module instance.
-}
{#fun unsafe ModuleInstanceGetHostData as ^ 
  {`ModuleInstanceContext'              -- ^ the WasmEdge_ModuleInstanceContext.
  } -> `HsRef'toHsRefFromVoidPtrOut*    -- ^ host data. NULL if the module instance context is NULL or no host data set into the module instance.
#}

{-|
  Get the exported function instance context of a module instance.
 
  The result function instance context links to the function instance in the
  module instance context and owned by the module instance context, and the
  caller should __NOT__ call the `WasmEdge_FunctionInstanceDelete`.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceFindFunction as ^ 
  {`ModuleInstanceContext'        -- ^ the WasmEdge_ModuleInstanceContext.
  ,%`WasmString'                  -- ^ the function name WasmEdge_String.  
  } -> `Maybe FunctionInstanceContext'nullableNoFinalizer*  -- ^ pointer to the function instance context. NULL if not found.
#}

{-|
  Get the exported table instance context of a module instance.
 
  The result table instance context links to the table instance in the module
  instance context and owned by the module instance context, and the caller
  should __NOT__ call the `WasmEdge_TableInstanceDelete`.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceFindTable as ^ 
  {`ModuleInstanceContext'        -- ^ the WasmEdge_ModuleInstanceContext.
  ,%`WasmString'                  -- ^ the table name WasmEdge_String.
  } -> `TableInstanceContext'     -- ^ pointer to the table instance context. NULL if not found.
#}

{-|
  Get the exported memory instance context of a module instance.
 
  The result memory instance context links to the memory instance in the
  module instance context and owned by the module instance context, and the
  caller should __NOT__ call the `WasmEdge_MemoryInstanceDelete`.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceFindMemory as ^ 
  {`ModuleInstanceContext'        -- ^ the WasmEdge_ModuleInstanceContext.
  ,%`WasmString'                  -- ^ the memory name WasmEdge_String.
  } -> `MemoryInstanceContext'    -- ^ pointer to the memory instance context. NULL if not found.
#}

{-|
  Get the exported global instance context of a module instance.
 
  The result global instance context links to the global instance in the
  module instance context and owned by the module instance context, and the
  caller should __NOT__ call the `WasmEdge_GlobalInstanceDelete`.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceFindGlobal as ^ 
  {`ModuleInstanceContext'        -- ^ the WasmEdge_ModuleInstanceContext.
  ,%`WasmString'                  -- ^ the global name WasmEdge_String.
  } -> `GlobalInstanceContext'    -- ^ pointer to the global instance context. NULL if not found.
#}

{-|
  Get the length of exported function list of a module instance.
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceListFunctionLength as ^ 
  {`ModuleInstanceContext'       -- ^ the WasmEdge_ModuleInstanceContext.
  } -> `Word32'                  -- ^ length of the exported function list.
#}

{-|
  List the exported function names of a module instance.
 
  The returned function names filled into the `Names` array are linked to the
  exported names of functions of the module instance context, and the caller
  should __NOT__ call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the exported
  function list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceListFunctionOut as moduleInstanceListFunction_ 
  {`ModuleInstanceContext'                              -- ^ the WasmEdge_ModuleInstanceContext.
  , fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&     -- ^ [out] Names the output WasmEdge_String buffer of the function names and length of the buffer
  } -> `Word32'                                         -- ^ actual exported function list size.
#}

moduleInstanceListFunction ::
  ModuleInstanceContext
  -> Word32
  -> IO (V.Vector WasmString)
moduleInstanceListFunction modInst retLen = do
  retOut <- VSM.generateM (fromIntegral retLen) (const $ allocWasmString pure)
  res <- moduleInstanceListFunction_ modInst retOut
  rets <- V.generateM (fromIntegral res) ((useFinalizerFree =<<) . (VSM.read retOut))
  pure rets
  

{-|
  Get the length of exported table list of a module instance.
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceListTableLength as ^ 
  {`ModuleInstanceContext'                      -- ^ the WasmEdge_ModuleInstanceContext.
  } -> `Word32'                                 -- ^ length of the exported table list.
#}

{-|
  List the exported table names of a module instance.
 
  The returned table names filled into the `Names` array are linked to the
  exported names of tables of the module instance context, and the caller
  should __NOT__ call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the exported
  table list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceListTableOut as moduleInstanceListTable 
  {`ModuleInstanceContext'                            -- ^ the WasmEdge_ModuleInstanceContext.
  , fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&   -- ^ [out] Names the output WasmEdge_String buffer of the table names and the length of the buffer
  } -> `Word32'                                       -- ^ actual exported table list size.
  #}

{-|
  Get the length of exported memory list of a module instance.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceListMemoryLength as ^ 
  {`ModuleInstanceContext'          -- ^ the WasmEdge_ModuleInstanceContext.
  } -> `Word32'                     -- ^ length of the exported memory list.
#} 

{-|
  List the exported memory names of a module instance.
 
  The returned memory names filled into the `Names` array are linked to the
  exported names of memories of the module instance context, and the caller
  should __NOT__ call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the exported
  memory list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceListMemoryOut as moduleInstanceListMemory 
  {`ModuleInstanceContext'                              -- ^ the WasmEdge_ModuleInstanceContext. 
  , fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&     -- ^ [out] Names the output WasmEdge_String buffer of the memory names and length of the buffer
  } -> `Word32'                                         -- ^ actual exported memory list size.
#} 

{-|
  Get the length of exported global list of a module instance.
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceListGlobalLength as ^ 
  {`ModuleInstanceContext'      -- ^  the WasmEdge_ModuleInstanceContext.
  } -> `Word32'                 -- ^ length of the exported global list.
#} 

{-|
  List the exported global names of a module instance.
 
  The returned global names filled into the `Names` array are linked to the
  exported names of globals of the module instance context, and the caller
  should __NOT__ call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the exported
  global list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceListGlobalOut as moduleInstanceListGlobal 
  {`ModuleInstanceContext'                            -- ^ the WasmEdge_ModuleInstanceContext.
  , fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&   -- ^ [out] Names the output WasmEdge_String buffer of the global names and the length of the buffer
  } -> `Word32'                                       -- ^ actual exported global list size.
  #} 

{-|
  Add a function instance context into a WasmEdge_ModuleInstanceContext.
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceAddFunction as ^ 
  {`ModuleInstanceContext'          -- ^ the WasmEdge_ModuleInstanceContext to add the function instance.
  ,%`WasmString'                    -- ^ the export function name WasmEdge_String.
  ,`FunctionInstanceContext'        -- ^ the WasmEdge_FunctionInstanceContext to add.
  } -> `()'
#}

{-|
  Add a table instance context into a WasmEdge_ModuleInstanceContext.
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceAddTable as ^ 
  {`ModuleInstanceContext'          -- ^ the WasmEdge_ModuleInstanceContext to add the table instance.
  ,%`WasmString'                    -- ^ the export table name WasmEdge_String.
  ,`TableInstanceContext'           -- ^ the WasmEdge_TableInstanceContext to add.
  } -> `()'
#}

{-|
  Add a memory instance context into a WasmEdge_ModuleInstanceContext.
  This function is thread-safe.
-}
{#fun unsafe ModuleInstanceAddMemory as ^ 
  {`ModuleInstanceContext'        -- ^ the WasmEdge_ModuleInstanceContext to add the memory instance.
  ,%`WasmString'                  -- ^ the export memory name WasmEdge_String.
  ,`MemoryInstanceContext'        -- ^ the WasmEdge_MemoryInstanceContext to add.
  } -> `()'
#}

{-|
  Add a global instance context into a WasmEdge_ModuleInstanceContext.
 This function is thread-safe.
-}
{#fun unsafe ModuleInstanceAddGlobal as ^ 
  {`ModuleInstanceContext'        -- ^ the WasmEdge_ModuleInstanceContext to add the global instance.
  ,%`WasmString'                  -- ^ the export global name WasmEdge_String.
  ,`GlobalInstanceContext'        -- ^ the WasmEdge_GlobalInstanceContext to add.
  } -> `()'
#}

-- Function Instance
{#pointer HostFunc_t as ^#}
type HostFun = Ptr HsRefPtr -> Ptr (CallingFrameContext) -> Ptr (Ptr WasmVal) -> Ptr (Ptr WasmVal) -> IO (Ptr WasmResult)

foreign import ccall "wrapper" toHostFuncT :: HostFun -> IO HostFuncT

-- | Creates WasmEdge HostFunc callback reference
hostFuncCallback ::
  Word32 -- ^ Parameter count
  -> Word32 -- ^ Returned values count
  -> (Maybe HsRef -> CallingFrameContext -> V.Vector WasmVal -> IO (V.Vector WasmVal)) -- ^ Host Callback
  -> IO HostFuncT -- ^ WasmEdge HostFunc callback reference
hostFuncCallback parCnt retCnt cb = toHostFuncT $ \pHsRef pCFCxt pPars pRess -> do
  hsRefMay <- if pHsRef == nullPtr
              then pure Nothing
              else fmap Just $ toHsRefOut pHsRef
  ccxt <- CallingFrameContext <$> newForeignPtr_ pCFCxt
  let
    toVecWasmVal pVals = do
      wasmVals <- mapM ((fmap WasmVal) . newForeignPtr finalizerFree) pVals
      pure $ V.fromList wasmVals
    toResValPtrs :: V.Vector WasmVal -> IO [Ptr WasmVal]
    toResValPtrs = mapM (flip withWasmVal pure) . V.toList 

  pars <- toVecWasmVal =<< peekArray (fromIntegral parCnt) pPars
  ress <- cb hsRefMay ccxt pars
  if V.length ress == (fromIntegral retCnt)
    then do
    pokeArray pRess =<< toResValPtrs ress
    pure nullPtr -- WRSuccess -- TODO: Fix Status Ret by making it as out ptr
    else pure nullPtr -- WRFail -- TODO: Fix Status Ret by making it as out ptr

-- | Creates WasmEdge HostFunc callback reference
hostFuncCallbackPure ::
  Word32 -- ^ Parameter count
  -> Word32 -- ^ Returned values count
  -> (Maybe HsRef -> CallingFrameContext -> V.Vector WasmVal -> V.Vector WasmVal) -- ^ Host Callback
  -> IO HostFuncT -- ^ WasmEdge HostFunc callback reference
hostFuncCallbackPure parCnt retCnt cb = hostFuncCallback parCnt retCnt $ \ref cf pars -> pure $ cb ref cf pars

{-|
  Creation of the WasmEdge_FunctionInstanceContext for host functions.
-}
{#fun FunctionInstanceCreateBndr as functionInstanceCreate 
  {`FunctionTypeContext'          -- ^ the function type context to describe the host function signature
  ,`HostFuncT'                    -- ^ the host function pointer. 
  ,fromHsRefIn*`HsRef'            -- ^ the additional object, such as the pointer to a data structure, to set to this host function context. The caller should guarantee the life cycle of the object. NULL if the additional data object is not needed.
  ,`Word64'                       -- ^ the function cost in statistics. Pass 0 if the calculation is not needed
  } -> `Maybe FunctionInstanceContext'nullableFinalizablePtrOut*  -- ^ pointer to context, NULL if failed.
#}

-- TODO:
--{#fun unsafe FunctionInstanceCreateBinding as ^ {`FunctionTypeContext',`WrapFuncT',fromHsRefAsVoidPtrIn*`HsRef',fromHsRefAsVoidPtrIn*`HsRef',`Word64'} -> `FunctionInstanceContext'#}

{-|
  Get the function type context of the function instance.
-}
{#fun unsafe FunctionInstanceGetFunctionType as ^ 
  {`FunctionInstanceContext'      -- ^ the WasmEdge_FunctionInstanceContext.
  } -> `Maybe FunctionTypeContext'nullableNoFinalizer*  -- ^ pointer to context, NULL if failed.
#}

-- Table Instance
{-|
  Creation of the WasmEdge_TableInstanceContext.
-}
{#fun unsafe TableInstanceCreate as ^ 
  {`TableTypeContext'           -- ^ the table type context to initialize the table instance context.
  } -> `Maybe TableInstanceContext'nullableFinalizablePtrOut*  -- ^ pointer to context, NULL if failed.
#}

{-|
  Get the table type context from a table instance.
-}
{#fun unsafe TableInstanceGetTableType as ^ 
  {`TableInstanceContext'                 -- ^ the WasmEdge_TableInstanceContext.
  } -> `Maybe TableTypeContext'nullableNoFinalizer* -- ^ pointer to context, NULL if failed.
#}

{-|
  Get the reference value in a table instance.
 
  \param Cxt the WasmEdge_TableInstanceContext.
  \param [out] Data the result reference value.
  \param Offset the reference value offset (index) in the table instance.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe TableInstanceGetDataOut as tableInstanceGetData {+,`TableInstanceContext',allocWasmVal-`WasmVal'useFinalizerFree*,`Word32'} -> `WasmResult'#}

{-|
  Set the reference value into a table instance.
-}
{#fun unsafe TableInstanceSetDataOut as tableInstanceSetData  
  {+
  ,`TableInstanceContext'         -- ^ the WasmEdge_TableInstanceContext.
  ,`WasmVal'                      -- ^ the reference value to set into the table instance.
  ,`Word32'                       -- ^ the reference value offset (index) in the table instance.
  } -> `WasmResult'               -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Get the size of a table instance.
-}
{#fun unsafe TableInstanceGetSize as ^ 
  {`TableInstanceContext'           -- ^ the WasmEdge_TableInstanceContext.
  } -> `Word32'                     -- ^ the size of the table instance.
#} 

{-|
  Grow a table instance with a size.
-}
{#fun unsafe TableInstanceGrowOut as tableInstanceGrow 
  {+
  ,`TableInstanceContext'           -- ^ the WasmEdge_TableInstanceContext.
  ,`Word32'                         -- ^ the count of reference values to grow in the table instance.
  } -> `WasmResult'                 -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

-- Memory Instance
{-|
  Creation of the WasmEdge_MemoryInstanceContext.
-}
{#fun unsafe MemoryInstanceCreate as ^ 
  {`MemoryTypeContext'              -- ^ the memory type context to initialize the memory instance
  } -> `Maybe MemoryInstanceContext'nullableFinalizablePtrOut*  -- ^ pointer to context, NULL if failed.
#} 

{-|
  Get the memory type context from a memory instance.
-}
{#fun unsafe MemoryInstanceGetMemoryType as ^ 
  {`MemoryInstanceContext'  -- ^ the WasmEdge_MemoryInstanceContext.
  } -> `MemoryTypeContext'  -- ^ pointer to context, NULL if failed.
#} 
{#fun unsafe MemoryInstanceGetDataOut as memoryInstanceGetData_ {+,`MemoryInstanceContext',fromByteStringIn*`ByteString'&,`Word32'} -> `WasmResult'#}

{-|
Copy the data to the output buffer from a memory instance.
-}
memoryInstanceGetData ::
  MemoryInstanceContext
  -> Word32 -- ^ Length of the Buffer
  -> Word32 -- ^ the data start offset in the memory instance.
  -> IO (WasmResult, ByteString) -- ^ (the status, the result data buffer of copying destination)
memoryInstanceGetData micxt len off = do
  bs <- IntBS.create (fromIntegral len) (const $ pure ())
  wr <- memoryInstanceGetData_ micxt bs off
  pure (wr, bs)
  
-- TODO:
{-|
  Copy the data into a memory instance from the input buffer.
-}
{#fun unsafe MemoryInstanceSetDataOut as memoryInstanceSetData 
  {+
  ,`MemoryInstanceContext'            -- ^ the WasmEdge_MemoryInstanceContext.
  ,fromByteStringIn*`ByteString'&     -- ^ the data buffer to copy and the start offset in the memory instance
  ,`Word32'                           -- ^ the data start offset in the memory instance. If the `Offset + Length` is larger than the data size in the memory instance, this function will failed.
  } -> `WasmResult'                   -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{#fun unsafe MemoryInstanceGetPointer as memoryInstanceGetPointer_ {`MemoryInstanceContext',`Word32',`Word32'} -> `Ptr Word8'coercePtr#}

{-|
Get the data pointer in a memory instance.
-}
memoryInstanceGetPointer ::
  MemoryInstanceContext
  -> Word32 -- ^ Length of the Buffer
  -> Word32 -- ^ the data start offset in the memory instance.
  -> IO ByteString
memoryInstanceGetPointer micxt len off = (BS.packCStringLen . \pW8 -> (castPtr pW8, fromIntegral len)) =<< memoryInstanceGetPointer_ micxt off len

{#fun unsafe MemoryInstanceGetPointerConst as memoryInstanceGetPointerConst_ {`MemoryInstanceContext',`Word32',`Word32'} -> `Ptr Word8'coercePtr#}

memoryInstanceGetPointerConst ::
  MemoryInstanceContext
  -> Word32 -- ^ Length of the Buffer
  -> Word32 -- ^ the data start offset in the memory instance.
  -> IO ByteString
memoryInstanceGetPointerConst micxt len off = (BS.packCStringLen . \pW8 -> (castPtr pW8, fromIntegral len)) =<< memoryInstanceGetPointerConst_ micxt off len

{-|
  Get the current page size (64 KiB of each page) of a memory instance.
-}
{#fun unsafe MemoryInstanceGetPageSize as ^ 
  {`MemoryInstanceContext'     -- ^ the WasmEdge_MemoryInstanceContext.
  } -> `Word32'                -- ^ the page size of the memory instance.
#} 

{-|
  Grow a memory instance with a page size.
-}
{#fun unsafe MemoryInstanceGrowPageOut as memoryInstanceGrowPage 
  {+
  ,`MemoryInstanceContext'      -- ^ the WasmEdge_MemoryInstanceContext.
  ,`Word32'                     -- ^ the page count to grow in the memory instance.
  } -> `WasmResult'             -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#} 

-- Global Instance
{-|
  Creation of the WasmEdge_GlobalInstanceContext.
-}
{#fun unsafe GlobalInstanceCreateOut as globalInstanceCreate 
  {`GlobalTypeContext'        -- ^ the global type context to initialize the global instance context.
  ,`WasmVal'                  -- ^ the initial value with its value type of the global instance. 
  } -> `Maybe GlobalInstanceContext'nullableFinalizablePtrOut* -- ^ pointer to context, NULL if failed.
#}

{-|
  Get the global type context from a global instance.
-}
{#fun unsafe GlobalInstanceGetGlobalType as ^ 
  {`GlobalInstanceContext'    -- ^ the WasmEdge_GlobalInstanceContext.
  } -> `Maybe GlobalTypeContext'nullableNoFinalizer* -- ^ pointer to context, NULL if failed.
#} 

{-|
  Get the value from a global instance.
-}
{#fun unsafe GlobalInstanceGetValueOut as globalInstanceGetValue 
  {+
  ,`GlobalInstanceContext'     -- ^ the WasmEdge_GlobalInstanceContext.      
  } -> `WasmVal'               -- ^ the current value of the global instance.
#}

{-|
  Set the value from a global instance.
 
  This function will do nothing if the global context is set as the `Const` mutation or the value type not matched.
-}
{#fun unsafe GlobalInstanceSetValueOut as globalInstanceSetValue 
  {`GlobalInstanceContext'      -- ^ the WasmEdge_GlobalInstanceContext.
  ,`WasmVal'                    -- ^ the value to set into the global context.
  } -> `()'
#} 

-- Calling Frame
{-|
  Get the executor context from the current calling frame.
-}
{#fun unsafe CallingFrameGetExecutor as ^ 
  {`CallingFrameContext'      -- ^ the WasmEdge_CallingFrameContext.
  } -> `ExecutorContext'      -- ^ the executor context, NULL if the Cxt is NULL.
#}

{-|
  Get the module instance of the current calling frame.
 
  When a WASM function is executing and start to call a host function, a frame with the module instance which the WASM function belongs to will be pushed onto the stack. 
  And therefore the calling frame context will record that module instance.
  So in one case that the module instance will be `NULL`: developers execute the function instance which is a host function and not added into a module instance.
-}
{#fun unsafe CallingFrameGetModuleInstance as ^ 
  {`CallingFrameContext'        -- ^ the WasmEdge_CallingFrameContext.
  } -> `ModuleInstanceContext'  -- ^ the module instance of the current calling frame.
#}

{-|
  Get the memory instance by index from the module instance of the current calling frame.
  By default, a WASM module only have one memory instance after instantiation.
-}
{#fun unsafe CallingFrameGetMemoryInstance as ^ 
  {`CallingFrameContext'        -- ^ the WasmEdge_CallingFrameContext.
  ,`Word32'                     -- ^ the index of memory instance in the module instance.  
  } -> `MemoryInstanceContext'  -- ^ the memory instance, NULL if not found.
#}

-- Async

{-|
  Wait a WasmEdge_Async execution.
-}
{#fun AsyncWait as ^ 
  {`Async'                  -- ^ the WasmEdge_ASync.
  } -> `()'
#}

{-|
  Wait a WasmEdge_Async execution with timeout.
-}
{#fun AsyncWaitFor as ^ 
  {`Async'                    -- ^ the WasmEdge_ASync.
  ,`Word64'                   -- ^ Milliseconds times to wait.
  } -> `Bool'                 -- ^ Result of waiting, true for execution ended, false for timeout occurred.
#}

{- |
  Cancel a WasmEdge_Async execution.
-}
{#fun AsyncCancel as ^ 
  {`Async'                        -- ^ the WasmEdge_ASync.
  } -> `()'
#}

{-|
  Wait and get the return list length of the WasmEdge_Async execution.
 
  This function will wait until the execution finished and return the return value list length of the executed function. 
  This function will return 0 if the `Cxt` is NULL, the execution was failed, or the execution was canceled. Developers can call the `WasmEdge_AsyncGet` to get the execution status and the return values.
-}
{#fun AsyncGetReturnsLength as ^ 
  {`Async'                          -- ^ the WasmEdge_ASync.
  } -> `Word32'                     -- ^ the return list length of the executed function.
#}

{-|
  Wait and get the result of WasmEdge_Async execution.
 
  This function will wait until the execution finished and return the
  execution status and the return values.
  If the `Returns` buffer length is smaller than the arity of the function,
  the overflowed return values will be discarded.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_ASync.
  \param [out] Returns the WasmEdge_Value buffer to fill the return values.
  \param ReturnLen the return buffer length.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun AsyncGetOut as asyncGet_
 {+
 ,`Async'
 ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&
 } -> `WasmResult'#}

{-|
  Wait and get the result of WasmEdge_Async execution.
 
  This function will wait until the execution finished and return the
  execution status and the return values.
  If the `Returns` buffer length is smaller than the arity of the function,
  the overflowed return values will be discarded.
-}
asyncGet ::
  Async -- ^ the WasmEdge_ASync.
  -> Word32 -- ^ the return buffer length.
  -> IO (WasmResult, V.Vector WasmVal) -- ^ the result status & the return values.
asyncGet async retLen = do
  retOut <- VSM.generateM (fromIntegral retLen) (const $ allocWasmVal pure)
  res <- asyncGet_ async retOut
  rets <- V.generateM (fromIntegral retLen) ((useFinalizerFree =<<) . (VSM.read retOut))
  pure (res, rets)

-- VM
{-|
  Register a module instance into the store in VM with exporting its module name.
  After calling this function, the existing module instance will be registered into 
  the store context in this VM, and the other modules can import the exported instances for linking when instantiation.
-}
{#fun unsafe VMRegisterModuleFromImportOut as vMRegisterModuleFromImport 
  {+
  ,`VMContext'                  -- ^ the WasmEdge_VMContext which contains the store.
  ,`ModuleInstanceContext'      -- ^ the WasmEdge_ModuleInstanceContext to register.
  } -> `WasmResult'             -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Instantiate the WASM module from a WASM file and invoke a function by name.
 
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from the file path, and then invoke a function by name and parameters. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded. After calling this function, a new module instance is instantiated, and the old one will be destroyed.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
  \param Path the NULL-terminated C string of the WASM file path.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
  \param [out] Returns the WasmEdge_Value buffer to fill the return values.
  \param ReturnLen the return buffer length.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}

{#fun unsafe VMRunWasmFromFileOut as vMRunWasmFromFile_ {+,`VMContext',`String',%`WasmString',fromVecOfFPtr*`V.Vector WasmVal'&,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `WasmResult'#}


{-|
  Instantiate the WASM module from a WASM file and invoke a function by name.
 
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from the file path, and then invoke a function by name and parameters. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded. After calling this function, a new module instance is instantiated, and the old one will be destroyed.
 
  This function is thread-safe.
-}
vMRunWasmFromFile ::
  VMContext -- ^ the WasmEdge_VMContext.
  -> String -- ^ the WASM file path.
  -> WasmString -- ^ the function name.
  -> V.Vector WasmVal -- ^ the parameter values.
  -> Word32 -- ^ the return buffer length.
  -> IO (WasmResult, V.Vector WasmVal) -- ^ the result status & the return values.
vMRunWasmFromFile cxt fp fname args retLen = do
  retOut <- VSM.generateM (fromIntegral retLen) (const $ allocWasmVal pure)
  res <- vMRunWasmFromFile_ cxt fp fname args retOut
  rets <- V.generateM (fromIntegral retLen) ((useFinalizerFree =<<) . (VSM.read retOut))
  pure (res, rets)
  
  


{-|
  Instantiate the WASM module from a buffer and invoke a function by name.
 
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from a buffer, and then invoke a function by name and parameters. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded. After calling this function, a new module instance is instantiated, and the old one will be destroyed.
-}
{#fun unsafe VMRunWasmFromBufferOut as vMRunWasmFromBuffer_ 
  {+
  ,`VMContext'                                    -- ^ the WasmEdge_VMContext.
  ,fromByteStringIn*`ByteString'&                 -- ^ the buffer of WASM binary and the length of the buffer
  ,%`WasmString'                                  -- ^ the function name WasmEdge_String 
  ,fromVecOfFPtr*`V.Vector WasmVal'&              -- ^ the WasmEdge_Value buffer with the parameter values and the length
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&   -- ^  [out] Returns the WasmEdge_Value buffer to fill the return values and the length of the buffer
  } -> `WasmResult'                               -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Instantiate the WASM module from a buffer and invoke a function by name.
 
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from a buffer, and then invoke a function by name and parameters. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded. After calling this function, a new module instance is instantiated, and the old one will be destroyed.
-}
vMRunWasmFromBuffer ::
  VMContext -- ^ the WasmEdge_VMContext.
  -> ByteString -- ^ the buffer of WASM binary.
  -> WasmString -- ^ the function name.
  -> V.Vector WasmVal -- ^ the parameter values.
  -> Word32 -- ^ the return buffer length.
  -> IO (WasmResult, V.Vector WasmVal) -- ^ the result status & the return values.
vMRunWasmFromBuffer cxt wasmBuff fname args retLen = do
  retOut <- VSM.generateM (fromIntegral retLen) (const $ allocWasmVal pure)
  res <- vMRunWasmFromBuffer_ cxt wasmBuff fname args retOut
  rets <- V.generateM (fromIntegral retLen) ((useFinalizerFree =<<) . (VSM.read retOut))
  pure (res, rets)

{-|
  Instantiate the WASM module from a WasmEdge AST Module and invoke a function by name.
 
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from the WasmEdge AST Module, and then invoke the function by name and parameters. If the `Returns` buffer length is smaller than the arity of the function, 
  the overflowed return values will be discarded.  After calling this function, a new module instance is instantiated, and the old one will be destroyed.
 
  This function is thread-safe.
-}
{#fun unsafe VMRunWasmFromASTModuleOut as vMRunWasmFromASTModule_ 
  {+
  ,`VMContext'                                      -- ^ the WasmEdge_VMContext.
  ,`ASTModuleContext'                               -- ^ the WasmEdge AST Module context generated by loader or compiler.
  ,%`WasmString'                                    -- ^ the function name WasmEdge_String.
  ,fromVecOfFPtr*`V.Vector WasmVal'&                -- ^ the WasmEdge_Value buffer with the parameter values and the buffer length
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&     -- ^ [out] Returns the WasmEdge_Value buffer to fill the return values and the buffer len
  } -> `WasmResult'                                 -- ^  WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Instantiate the WASM module from a WasmEdge AST Module and invoke a function by name.
 
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from the WasmEdge AST Module, and then invoke the function by name and parameters. If the `Returns` buffer length is smaller than the arity of the function, 
  the overflowed return values will be discarded.  After calling this function, a new module instance is instantiated, and the old one will be destroyed.
 
  This function is thread-safe.
-}
vMRunWasmFromASTModule ::
  VMContext -- ^ the WasmEdge_VMContext.
  -> ASTModuleContext -- ^ the WasmEdge AST Module context generated by loader or compiler.
  -> WasmString -- ^ the function name.
  -> V.Vector WasmVal -- ^ the parameter values.
  -> Word32 -- ^ the return buffer length.
  -> IO (WasmResult, V.Vector WasmVal) -- ^ the result status & the return values.
vMRunWasmFromASTModule cxt astMod fname args retLen = do
  retOut <- VSM.generateM (fromIntegral retLen) (const $ allocWasmVal pure)
  res <- vMRunWasmFromASTModule_ cxt astMod fname args retOut
  rets <- V.generateM (fromIntegral retLen) ((useFinalizerFree =<<) . (VSM.read retOut))
  pure (res, rets)

{-|
  Instantiate the WASM module from a WASM file and asynchronous invoke a function by name.
  
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from the file path, and then invoke a function by name and parameters. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded. After calling this function, a new module instance is instantiated, and the old one will be destroyed.
  
  This function is thread-safe.
-}
{#fun VMAsyncRunWasmFromFileOut as vMAsyncRunWasmFromFile 
  {`VMContext'                                     -- ^ the WasmEdge_VMContext.
  ,`String'                                        -- ^ the WASM file path.
  ,%`WasmString'                                   -- ^ the function name WasmEdge_String.
  ,fromVecOfFPtr*`V.Vector WasmVal'&               -- ^ the parameter values
  } -> `Async'                                     -- ^ WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call `WasmEdge_AsyncDelete` to destroy this object.
#}

{-|
  Instantiate the WASM module from a WasmEdge AST Module and asynchronous invoke a function by name.
 
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from the WasmEdge AST Module, and then invoke the function by name and parameters. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded. After calling this function, a new module instance is instantiated, and the old one will be destroyed.

  This function is thread-safe.
-}
{#fun unsafe VMAsyncRunWasmFromASTModuleOut as vMAsyncRunWasmFromASTModule 
  {`VMContext'                                          -- ^ the WasmEdge_VMContext.
  ,`ASTModuleContext'                                   -- ^ the WasmEdge AST Module context generated by loader or compiler.
  ,%`WasmString'                                        -- ^ the function name WasmEdge_String.
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&         -- ^ the WasmEdge_Value buffer with the parameter values and the parameter buffer length.
  } -> `Async'                                          -- ^ WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call `WasmEdge_AsyncDelete` to destroy this object.
#}

{-|
  Instantiate the WASM module from a buffer and asynchronous invoke a function by name.
 
  This is the function to invoke a WASM function rapidly. Load and instantiate the WASM module from a buffer, and then invoke a function by name and parameters. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded. After calling this function, a new module instance is instantiated, and the old one will be destroyed.
 
  This function is thread-safe.
-}
{#fun unsafe VMAsyncRunWasmFromBufferOut as vMAsyncRunWasmFromBuffer 
  {`VMContext'                                        -- ^ the WasmEdge_VMContext.
  ,fromByteStringIn*`ByteString'&                     -- ^ the buffer of WASM binary and the length of the buffer.
  , %`WasmString'                                     -- ^ the function name WasmEdge_String.
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&        -- ^ the WasmEdge_Value buffer with the parameter values and the parameter buffer length
  } -> `Async'                                        -- ^ WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call `WasmEdge_AsyncDelete` to destroy this object.
#}

{-|
  Creation of the WasmEdge_VMContext.
 
  The caller owns the object and should call `WasmEdge_VMDelete` to destroy
  it.
 
  \param ConfCxt the WasmEdge_ConfigureContext as the configuration of VM.
  NULL for the default configuration.
  \param StoreCxt the WasmEdge_StoreContext as the external WASM store of VM.
  The instantiation and execution will refer to this store context, and the
  life cycle should be ensured until the VM context is deleted. NULL for the
  default store owned by `WasmEdge_VMContext`.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe VMCreate as ^ {`ConfigureContext',nullablePtrIn*`Maybe StoreContext'} -> `Maybe VMContext'nullableFinalizablePtrOut*#}

{-|
  Register and instantiate WASM into the store in VM from a WASM file.
 
  Load a WASM file from the path, and register all exported instances and instantiate them into the store into the VM with their exported name and module name.
 
  This function is thread-safe.
-}
{#fun unsafe VMRegisterModuleFromFileOut as vMRegisterModuleFromFile 
  {+
  ,`VMContext'                  -- ^ the WasmEdge_VMContext which contains the store.
  ,%`WasmString'                -- ^ the WasmEdge_String of module name for all exported instances.
  ,`String'                     -- ^ the NULL-terminated C string of the WASM file path.
  } -> `WasmResult'             -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Register and instantiate WASM into the store in VM from a buffer.
 
  Load a WASM module from a buffer, and register all exported instances and instantiate them into the store into the VM with their exported name and module name.
 
  This function is thread-safe.
-}
{#fun unsafe VMRegisterModuleFromBufferOut as vMRegisterModuleFromBuffer 
  {+
  ,`VMContext'                      -- ^ the WasmEdge_VMContext which contains the store.
  ,%`WasmString'                    -- ^ the WasmEdge_String of module name for all exported instances.
  ,fromByteStringIn*`ByteString'&   -- ^ the buffer of WASM binary and the length of the buffer
  } -> `WasmResult'                 -- ^ WasmResult
#}

{-|
  Instantiate and register an AST Module into a named module instance in VM.
 
  Load from the AST Module, and register all exported instances and instantiate them into the store in VM with their exported name and module name.
 
  This function is thread-safe.
-}
{#fun unsafe VMRegisterModuleFromASTModuleOut as vMRegisterModuleFromASTModule 
  {+
  ,`VMContext'                        -- ^ the WasmEdge_VMContext which contains the store.
  ,%`WasmString'                      -- ^ the WasmEdge_String of module name for all exported instances.
  ,`ASTModuleContext'                 -- ^ the WasmEdge AST Module context generated by loader or
  } -> `WasmResult'                   -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Load the WASM module from a WASM file.
 
  This is the first step to invoke a WASM function step by step. Load and parse the WASM module from the file path. You can then call `WasmEdge_VMValidate` for the next step.
 
  This function is thread-safe.
-}
{#fun unsafe VMLoadWasmFromFileOut as vMLoadWasmFromFile 
  {+
  ,`VMContext'                        -- ^ the WasmEdge_VMContext.
  ,`String'                           -- ^ the NULL-terminated C string of the WASM file path.
  } -> `WasmResult'                   -- ^ WasmEdge_Result
#}

{-|
  Load the WASM module from a buffer.
 
  This is the first step to invoke a WASM function step by step. Load and parse the WASM module from a buffer. You can then call `WasmEdge_VMValidate` for the next step.
 
  This function is thread-safe.
-}
{#fun unsafe VMLoadWasmFromBufferOut as vMLoadWasmFromBuffer 
  {+
  ,`VMContext'                        -- ^ the WasmEdge_VMContext.
  ,fromByteStringIn*`ByteString'&     -- ^ the buffer of WASM binary and the length of the buffer
  } -> `WasmResult'
#}

{-|
  Load the WASM module from loaded WasmEdge AST Module.
 
  This is the first step to invoke a WASM function step by step. Copy the loaded WasmEdge AST Module context into VM. 
  The VM context has no dependency on the input AST Module context. You can then call `WasmEdge_VMValidate` for the next step.
 
  This function is thread-safe.
-}
{#fun unsafe VMLoadWasmFromASTModuleOut as vMLoadWasmFromASTModule 
  {+
  ,`VMContext'                         -- ^ the WasmEdge_VMContext.
  ,`ASTModuleContext'                  -- ^ the WasmEdge AST Module context generated by loader or compiler.
  } -> `WasmResult'
#}

{-|
  Validate the WASM module loaded into the VM context.
 
  This is the second step to invoke a WASM function step by step. After loading a WASM module into VM context, You can call this function to validate it. 
  And you can then call `WasmEdge_VMInstantiate` for the next step. Note that only validated WASM modules can be instantiated in the VM context.
 
  This function is thread-safe.
-}
{#fun unsafe VMValidateOut as vMValidate  
  {+,
  `VMContext'                           -- ^ the WasmEdge_VMContext.
  } -> `WasmResult'                     -- ^ WasmResult
#}

{-
  Instantiate the validated WASM module in the VM context.
 
  This is the third step to invoke a WASM function step by step. After validating a WASM module in the VM context, You can call this function to instantiate it. 
  And you can then call `WasmEdge_VMExecute` for invoking the exported function in this WASM module. 
  After calling this function, a new module instance is instantiated, and the old one will be destroyed.
 
  This function is thread-safe.
-}
{#fun unsafe VMInstantiateOut as vMInstantiate  
  {+
  ,`VMContext'                                -- ^ the WasmEdge_VMContext.
  } -> `WasmResult'                           -- ^ WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error message.
#}

{-|
  Invoke a WASM function by name.
 
  This is the final step to invoke a WASM function step by step. After instantiating a WASM module in the VM context, the WASM module is registered into the store in the VM context as an anonymous module. 
  Then you can repeatedly call this function to invoke the exported WASM functions by their names until the VM context is reset or a new WASM module is registered or loaded. For calling the functions in registered WASM modules with module names, please use `WasmEdge_VMExecuteRegistered` instead. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded.
 
  This function is thread-safe.
-}
{#fun unsafe VMExecuteOut as vMExecute 
  {+
  ,`VMContext'                                    -- ^ the WasmEdge_VMContext.
  ,%`WasmString'                                  -- ^ the function name WasmEdge_String.  
  , fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&  -- ^ the WasmEdge_Value buffer with the parameter values and the parameter buffer length.
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&   -- ^ [out] Returns the WasmEdge_Value buffer to fill the return values and the return buffer length.
  } -> `WasmResult'
#}

{-|
  Invoke a WASM function by its module name and function name.
 
  After registering a WASM module in the VM context, you can repeatedly call this function to invoke exported WASM functions by their module names and function names until the VM context is reset. 
  If the `Returns` buffer length is smaller than the arity of the function, the overflowed return values will be discarded.
-}
{#fun unsafe VMExecuteRegisteredOut as vMExecuteRegistered 
  {+                                             
  ,`VMContext'                                    -- ^ the WasmEdge_VMContext.
  ,%`WasmString'                                  -- ^ the module name WasmEdge_String.
  ,%`WasmString'                                  -- ^ the function name WasmEdge_String.
  , fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&  -- ^ the WasmEdge_Value buffer with the parameter values and the parameter buffer length
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&   -- ^ [out] Returns the WasmEdge_Value buffer to fill the return values.
  } -> `WasmResult'                               -- ^ WasmEdge_Result
#}

{-|
  Asynchronous invoke a WASM function by name.
 
  This is the final step to invoke a WASM function step by step. After instantiating a WASM module in the VM context, the WASM module is registered into the store in the VM context as an anonymous module. 
  Then you can repeatedly call this function to invoke the exported WASM functions by their names until the VM context is reset or a new WASM module is registered or loaded. 
  For calling the functions in registered WASM modules with module names, please use `WasmEdge_VMAsyncExecuteRegistered` instead.
 
  This function is thread-safe.
-}
{#fun unsafe VMAsyncExecuteOut as vMAsyncExecute 
  {`VMContext'                                     -- ^ the WasmEdge_VMContext.
  ,%`WasmString'                                   -- ^ the function name WasmEdge_String.
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&    -- ^ the WasmEdge_Value buffer with the parameter values and the parameter buffer length.
  } -> `Async'                                     -- ^ WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call `WasmEdge_AsyncDelete` to destroy this object.
#}

{-|
  Asynchronous invoke a WASM function by its module name and function name.
  After registering a WASM module in the VM context, you can repeatedly call this function to invoke exported WASM functions by their module names and function names until the VM context is reset.
  This function is thread-safe.
-}
{#fun unsafe VMAsyncExecuteRegisteredOut as vMAsyncExecuteRegistered 
  {`VMContext'                                        -- ^ the WasmEdge_VMContext.
  ,%`WasmString'                                      -- ^ the module name WasmEdge_String.
  ,%`WasmString'                                      -- ^ the function name WasmEdge_String.
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&       -- ^ the WasmEdge_Value buffer with the parameter values and the parameter length buffer 
  } -> `Async'                                        -- ^ WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call `WasmEdge_AsyncDelete` to destroy this object.
#} 

{-|
  Get the function type by function name.
 
  After instantiating a WASM module in the VM context, the WASM module is registered into the store in the VM context as an anonymous module. 
  Then you can call this function to get the function type by the exported function name until the VM context is reset or a new WASM module is registered or loaded. 
  For getting the function type of functions in registered WASM modules with module names, please use `WasmEdge_VMGetFunctionTypeRegistered` instead.
  
  This function is thread-safe.
-}
{#fun unsafe VMGetFunctionType as ^ 
  {`VMContext'                                         -- ^ the WasmEdge_VMContext.
  ,%`WasmString'                                       -- ^ the function name WasmEdge_String.
  } -> `FunctionTypeContext'                           -- ^ the function type. NULL if the function not found.
#}

{-|
  Get the function type by function name.
  After registering a WASM module in the VM context, you can call this function to get the function type by the functions' exported module names and function names until the VM context is reset.
  This function is thread-safe.
-}
{#fun unsafe VMGetFunctionTypeRegistered as ^ 
  {`VMContext',                                         -- ^ the WasmEdge_VMContext.
  %`WasmString',                                        -- ^ the module name WasmEdge_String.
  %`WasmString'                                         -- ^ the function name WasmEdge_String.
  } -> `FunctionTypeContext'                            -- ^ the function type. NULL if the function not found.
#}

{-|
  Reset of WasmEdge_VMContext.
  After calling this function, the statistics, loaded module, the instantiated instances, and the registered instances except the WASI and plug-ins will all be cleared.
  This function is thread-safe.
-}
{#fun unsafe VMCleanup as ^ 
  {`VMContext'                                          -- ^ the WasmEdge_VMContext to reset.
  } -> `()'
#} 

{-|
  Get the length of exported function list.
  This function is thread-safe.
-}
{#fun unsafe VMGetFunctionListLength as ^ 
  {`VMContext'                                          -- ^ the WasmEdge_VMContext.
  } -> `Word32'                                         -- ^ length of exported function list.
#} 


{#fun unsafe VMGetFunctionListOut as vmGetFunctionList_ 
  {`VMContext'
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&, fromMutIOVecOr0Ptr*`IOVector (Ptr FunctionTypeContext)'&
  } -> `Word32'
#}

{-|
Get the length of exported function list.
-}
vMGetFunctionList ::
  VMContext                                                 -- ^ the WasmEdge_VMContext.
  -> Word32                                                 -- ^ Names the output names WasmEdge_String buffer of exported functions and length of the buffer
  -> IO (V.Vector WasmString, V.Vector FunctionTypeContext) -- ^ actual exported function list size.
vMGetFunctionList vmcxt sz = do
  namesVSM <- VSM.new (fromIntegral sz)
  ftypesVSM <- VSM.new (fromIntegral sz)
  listSz <- vmGetFunctionList_ vmcxt namesVSM ftypesVSM
  names <- V.generateM (fromIntegral listSz) ((noFinalizer =<<) . (VSM.read namesVSM))
  ftypes <- V.generateM (fromIntegral listSz) ((noFinalizer =<<) . (VSM.read ftypesVSM))
  pure (names, ftypes)
  
{-|
Get the module instance corresponding to the WasmEdge_HostRegistration settings.
-}
{#fun unsafe VMGetImportModuleContext as ^ 
  {`VMContext',                                 -- ^ the WasmEdge_VMContext.
  `HostRegistration'                            -- ^ the host registration value to get the import module.                            
  } -> `ModuleInstanceContext'                  -- ^  the module instance context. NULL if not found.
#} 

{-|
  Get the current instantiated module in VM.
  This function is thread-safe.
-}
{#fun unsafe VMGetActiveModule as ^ 
  {`VMContext'                                  -- ^ the WasmEdge_VMContext. 
  } -> `ModuleInstanceContext'                  -- ^ the module instance context. NULL if not found.
#} 

{-|
  Get the registered module in VM by the module name.
  This function is thread-safe.
-}
{#fun unsafe VMGetRegisteredModule as ^ 
  {`VMContext'                                    -- ^ the WasmEdge_VMContext.
  ,%`WasmString'                                  -- ^ the module name WasmEdge_String.
  } -> `ModuleInstanceContext'                    -- ^ the module instance context. NULL if not found.
#}

{-|
  Get the length of registered module list in the WasmEdge_VMContext.
  This function is thread-safe.
-}
{#fun unsafe VMListRegisteredModuleLength as ^ 
  {`VMContext'                                    -- ^ the WasmEdge_VMContext.
  } -> `Word32'                                   -- ^ length of registered module list.
#} 

{-|
  List the registered module names in the WasmEdge_VMContext.
  If the `Names` buffer length is smaller than the result of the registered named module list size, the overflowed return values will be discarded.
  This function is thread-safe.
-}
{#fun unsafe VMListRegisteredModuleOut as vMListRegisteredModule 
  {`VMContext'                                     -- ^ the WasmEdge_VMContext.
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'& -- ^ WasmEdge_String buffer of the registered modules and length of the buffer
  } -> `Word32'                                    -- ^ actual registered module list size.
#} 

{-|
  Get the store context used in the WasmEdge_VMContext.
  This function is thread-safe.
-}
{#fun unsafe VMGetStoreContext as ^ 
  {`VMContext'                                     -- ^ the WasmEdge_VMContext
  } -> `StoreContext'                              -- ^ the store context.
#} 

{-|
  Get the loader context used in the WasmEdge_VMContext.
  This function is thread-safe.
 -}
{#fun unsafe VMGetLoaderContext as ^ 
  {`VMContext'                                     -- ^ the WasmEdge_VMContext.
  } -> `LoaderContext'                             -- ^ the loader context.
#} 

{-|
  Get the validator context used in the WasmEdge_VMContext.
  This function is thread-safe.
-}
{#fun unsafe VMGetValidatorContext as ^ 
  {`VMContext'                                      -- ^ the WasmEdge_VMContext.
  } -> `ValidatorContext'                           -- ^ the validator context.
#} 

{-|
  Get the executor context used in the WasmEdge_VMContext.
  This function is thread-safe.
-}
{#fun unsafe VMGetExecutorContext as ^ 
  {`VMContext'                                        -- ^ the WasmEdge_VMContext.
  } -> `ExecutorContext'                              -- ^ the executor context.
#} 

{-|
  Get the statistics context used in the WasmEdge_VMContext.
  This function is thread-safe.
-}
{#fun unsafe VMGetStatisticsContext as ^ 
  {`VMContext'                                          -- ^ the WasmEdge_VMContext
  } -> `StatisticsContext'                              -- ^ pointer to the statistics context.
#} 

-- Driver
{-|
  Entrypoint for the compiler tool.
  This function provides an entrypoint to the WasmEdge AOT compiler tool with
  the command line arguments.
-}
{#fun unsafe Driver_Compiler as ^ 
  {fromVecStringOr0Ptr*`V.Vector String'&               -- ^ Argc the argument count and argv vector
  } -> `Int32'                                          -- ^ the execution status.
#}

{-|
  Entrypoint for the runtime tool.
  This function provides an entrypoint to the WasmEdge runtime tool with the
  command line arguments.
-}
{#fun unsafe Driver_Tool as ^ 
  {fromVecStringOr0Ptr*`V.Vector String'&               -- ^ Argc the argument count and argv vector
  } -> `Int32'                                          -- ^ the execution status.
#}

{-|
  Entrypoint for the unified tool.
  This function provides an entrypoint to the WasmEdge unified tool with the
  command line arguments.
-}
{#fun unsafe Driver_UniTool as ^ 
  {fromVecStringOr0Ptr*`V.Vector String'&               -- ^ Argc the argument count and argv vector
  } -> `Int32'                                          -- ^ the execution status.
#}

-- Plugin Function

{-|
  Load plugins with the default search paths.
 
  The default paths are:
    1. The environment variable "WASMEDGE_PLUGIN_PATH".
    2. The "../plugin/" directory related to the WasmEdge installation path.
    3. The "wasmedge/" directory under the library path if the WasmEdge is
       installed under the "/usr".
-}
{#fun unsafe PluginLoadWithDefaultPaths as ^ 
  {} -> `()'
#} 

{-|
  Load the plugin with the given file or directory.
 
  For the given file path, this function will load the plug-in.
  For the given directory path, this function will load the plug-ins under the
  directory recursively.
-}
{#fun unsafe PluginLoadFromPath as ^ 
  {`String'                                             -- ^ the path to plug-in file or directory.
  } -> `()'
#}

{-|
  Get the length of loaded plug-in list.
-}
{#fun unsafe PluginListPluginsLength as ^ 
  {} -> `Word32'                                        -- ^ length of loaded plug-in list.
#} 

{-|
  List the loaded plug-ins with their names.
 -}
{#fun unsafe PluginListPluginsOut as pluginListPlugins 
  {fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&      -- ^ Names the output WasmEdge_String buffer of the function names and length of the buffer
  } -> `Word32'                                         -- ^ actual loaded plug-in list size.
#} 

{-|
  Find the loaded plug-in context by name.
-}
{#fun unsafe PluginFind as ^ 
  {%`WasmString'              -- ^ the plug-in name WasmEdge_String.
  } -> `PluginContext'        -- ^ pointer to the plug-in context. NULL if the plug-in not found.
#} 

{-|
  Get the plug-in name of the plug-in context.
-}
{#fun unsafe PluginGetPluginNameOut as pluginGetPluginName 
  {+,                                                      -- ^ WasmEdge_String* in which the result would be stored
  `PluginContext'                                          -- ^ the WasmEdge_PluginContext.
  } -> `WasmString'                                        -- ^ WasmString Which contains name of the plugin
#} 

{-|
  Get the length of module list in the plug-in context.
-}
{#fun unsafe PluginListModuleLength as ^ 
  {`PluginContext' -- ^ the WasmEdge_PluginContext to get the length of the module list.
  } -> `Word32' -- ^ length of module list.
#} 

{-|
  List the modules in the plug-in context with their names.
-}
{#fun unsafe PluginListModuleOut as pluginListModule 
  {`PluginContext'                                    -- ^ the WasmEdge_PluginContext to list the modules. 
  ,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&    -- ^ Names the output WasmEdge_String buffer of the function names and the Buffer length
  } -> `Word32'                                       -- ^ actual module list size of the plug-in.
#} 

{-|
  Create the module instance in the plug-in by the module name. By giving the module name, developers can retrieve the module in the plug-in and create the module instance. 
  The caller owns the object and should call `WasmEdge_ModuleInstanceDelete` to destroy it.
-}
{#fun unsafe PluginCreateModule as ^ 
  {`PluginContext'                   -- ^ the WasmEdge_PluginContext to retrieve and create module.
    ,%`WasmString'                   -- ^ the module name to retrieve.
  } -> `ModuleInstanceContext'       -- ^ pointer to the module instance context, NULL if the module name not found in the plug-in or the plug-in is not valid.
#} 

{-|
  Implement by plugins for returning the plugin descriptor.
  \returns the plugin descriptor.
-}
-- TODO:
-- {#fun unsafe Plugin_GetDescriptor as ^ {} -> `PluginDescriptor'#} 


-- * FFI Utils
{-# INLINE fromCStrToText #-}
fromCStrToText :: CString -> IO Text
fromCStrToText cs = T.fromPtr0 $ castPtr cs

{-# INLINE fromHsRefIn #-}
fromHsRefIn :: HsRef -> (Ptr HsRefPtr -> IO a) -> IO a
fromHsRefIn = fromHsRefGenIn

{-# INLINE fromHsRefAsVoidPtrIn #-}
fromHsRefAsVoidPtrIn :: HsRef -> (Ptr () -> IO a) -> IO a
fromHsRefAsVoidPtrIn = fromHsRefGenIn

{-# INLINE fromHsRefGenIn #-}
fromHsRefGenIn :: HsRef -> (Ptr p -> IO a) -> IO a
fromHsRefGenIn (HsRef fprnt sp) f = do
  fp <- mallocForeignPtrBytes {#sizeof HsRef#}
  withForeignPtr fp $ \p -> alloca @Fingerprint $ \pFing -> do
    poke pFing fprnt
    {#set HsRef.Fingerprint#} p (castPtr pFing)
    {#set HsRef.Ref#} p (castStablePtrToPtr sp)
    f p

{-# INLINE toHsRefOut #-}
toHsRefOut :: Ptr HsRefPtr -> IO HsRef
toHsRefOut hsr = do
  pFing <- {#get HsRef.Fingerprint#} hsr
  fprint <- peek @Fingerprint (castPtr pFing)
  r <- {#get HsRef.Ref#} hsr
  pure $ HsRef fprint (castPtrToStablePtr r)

{-# INLINE toHsRefFromVoidPtrOut #-}
toHsRefFromVoidPtrOut :: Ptr () -> IO HsRef
toHsRefFromVoidPtrOut = toHsRefOut . castPtr

{-# INLINE fromHsRefWithFinalzrIn #-}
fromHsRefWithFinalzrIn :: HsRef -> ((Ptr (), FunPtr (Ptr () -> IO ())) -> IO a) -> IO a
fromHsRefWithFinalzrIn hsRef f = do
  hsDataFinalzr <- finalizerHSData $ const (freeHsRef hsRef)
  fromHsRefAsVoidPtrIn hsRef $ \pRef -> f (pRef, hsDataFinalzr)

{-# INLINE allocI128 #-}
allocI128 :: Int128 -> (Ptr CULong -> IO a) -> IO a
allocI128 i128 f = alloca $ \p -> poke p i128 *> f (castPtr p)

{-# INLINE peekI128 #-}
peekI128 :: Ptr CULong -> IO Int128
peekI128 p = peek @Int128 (castPtr p) 

{-# INLINE coercePtr #-}
coercePtr :: Coercible a b => Ptr a -> Ptr b
coercePtr = castPtr

{-# INLINE useAsCStringLenBS #-}
useAsCStringLenBS :: ByteString -> ((CString, CUInt) -> IO a) -> IO a
useAsCStringLenBS bs f = BS.useAsCStringLen bs (\strLen -> f (fromIntegral <$> strLen))

unsafeUseAsCStringLenBS :: ByteString -> ((CString, CUInt) -> IO a) -> IO a
unsafeUseAsCStringLenBS bs f = UnsafeBS.unsafeUseAsCStringLen bs (\strLen -> f (fromIntegral <$> strLen))

{-# INLINE useAsPtrCUCharLenBS #-}
useAsPtrCUCharLenBS :: ByteString -> ((Ptr CUChar, CUInt) -> IO a) -> IO a
useAsPtrCUCharLenBS bs f = BS.useAsCStringLen bs (\strLen -> f (bimap convPtrCCharToPtrCUChar fromIntegral strLen))
  where
    convPtrCCharToPtrCUChar :: CString -> Ptr CUChar
    convPtrCCharToPtrCUChar = castPtr
    

_packCStringLenBS :: CString -> CUInt -> IO ByteString
_packCStringLenBS cstr len = BS.packCStringLen (cstr, fromIntegral len)

{-# INLINE packCStringBS #-}
packCStringBS :: CString -> IO ByteString
packCStringBS cstr = BS.packCString cstr

{-# INLINE fromStoreVecOr0Ptr #-}
fromStoreVecOr0Ptr :: (Storable a, Num n) => Vector a -> ((Ptr n, CUInt) -> IO b) -> IO b
fromStoreVecOr0Ptr v f
  | VS.null v = f (nullPtr, 0)
  | otherwise = VS.unsafeWith v $ \p -> f (castPtr p, fromIntegral $ VS.length v)

{-# INLINE fromVecOr0Ptr #-}
fromVecOr0Ptr :: (Num sz) => (a -> IO (Ptr c)) -> V.Vector a -> ((Ptr (Ptr c), sz) -> IO b) -> IO b
fromVecOr0Ptr getPtr v f
  | V.null v = f (nullPtr, 0)
  | otherwise = do
      ptrs <- VSM.generateM (fromIntegral $ V.length v) (getPtr . V.unsafeIndex v)
      r <- fromMutIOVecOr0Ptr ptrs f
      VSM.mapM_ free ptrs
      pure r

{-# INLINE fromVecStringOr0Ptr #-}
fromVecStringOr0Ptr :: (Num sz) => V.Vector String -> ((Ptr (Ptr CChar), sz) -> IO b) -> IO b
fromVecStringOr0Ptr = fromVecOr0Ptr newCString

{-# INLINE fromVecOfFPtr #-}
fromVecOfFPtr :: forall t sz b.(Coercible t (ForeignPtr t), Num sz) => V.Vector t -> ((Ptr (Ptr t), sz) -> IO b) -> IO b
fromVecOfFPtr v f
  | V.null v = f (nullPtr, 0)
  | otherwise = do
      ptrs <- VSM.generate (fromIntegral $ V.length v) (unsafeForeignPtrToPtr . coerce . V.unsafeIndex v)
      r <- fromMutIOVecOr0Ptr ptrs f
      -- Keep the ref of ptrs taken from vec alive transitively by keeping the vec alive
      maybe (pure ()) (touchForeignPtr . (coerce @t @(ForeignPtr t))) (v V.!? 0)
      pure r
      

{-# INLINE fromMutIOVecOr0Ptr #-}
fromMutIOVecOr0Ptr :: (Storable a, Num sz) => IOVector a -> ((Ptr a, sz) -> IO b) -> IO b
fromMutIOVecOr0Ptr v f
  | VSM.null v = f (nullPtr, 0)
  | otherwise = VSM.unsafeWith v $ \p -> f (p, fromIntegral $ VSM.length v)

{-# INLINE fromMutIOVecOfCEnumOr0Ptr #-}
fromMutIOVecOfCEnumOr0Ptr :: (Storable a, Enum a) => IOVector a -> ((Ptr CInt, CUInt) -> IO b) -> IO b
fromMutIOVecOfCEnumOr0Ptr v f
  | VSM.null v = f (nullPtr, 0)
  | otherwise = VSM.unsafeWith v $ \p -> f (castPtr p, fromIntegral $ VSM.length v)

{-# INLINE fromByteStringIn #-}
fromByteStringIn :: (Coercible Word8 w8, Num sz) => BS.ByteString -> ((Ptr w8, sz) -> IO b) -> IO b
fromByteStringIn bs f = UnsafeBS.unsafeUseAsCStringLen bs $ \(p, l) -> f (coercePtr (castPtr p :: Ptr Word8), fromIntegral l)

{-# INLINE _hsFinalizer #-}
_hsFinalizer :: (Coercible (ForeignPtr t) t, HasFinalizer t) => Ptr t -> IO t
_hsFinalizer p = coerce $ newForeignPtr getFinalizer p
#if TESTONLY
              <* testonly_hsOwned p
#endif

{-# INLINE noFinalizer #-}
noFinalizer :: (Coercible (ForeignPtr t) t) => Ptr t -> IO t
noFinalizer p = coerce $ newForeignPtr_ p
#if TESTONLY
              <* testonly_cOwned p
#endif    

{-# INLINE nullableNoFinalizer #-}
nullableNoFinalizer :: (Coercible (ForeignPtr t) t) => Ptr t -> IO (Maybe t)
nullableNoFinalizer p
  | p == nullPtr = pure Nothing
  | otherwise = (Just . coerce) <$> newForeignPtr_ p
#if TESTONLY  
                <* testonly_cOwned p
#endif                

{-# INLINE nullableFinalizablePtrOut #-}
nullableFinalizablePtrOut :: forall t.(Coercible (ForeignPtr t) t, HasFinalizer t) => Ptr t -> IO (Maybe t)
nullableFinalizablePtrOut p
  | p == nullPtr = pure Nothing
  | otherwise = (Just . coerce) <$> newForeignPtr (getFinalizer @t) p
#if TESTONLY  
                <* testonly_hsOwned p
#endif                


{-# INLINE useFinalizerFree #-}
useFinalizerFree :: (Coercible (ForeignPtr t) t) => Ptr t -> IO t
useFinalizerFree p = coerce $ newForeignPtr finalizerFree p
#if TESTONLY  
                     <* testonly_hsOwned p
#endif

{-# INLINE useCAndFinalizerFree #-}
useCAndFinalizerFree :: forall t.(Coercible (ForeignPtr t) t, HasFinalizer t) => Ptr t -> IO t
useCAndFinalizerFree p = do
  fp <- newForeignPtr_ p
  addForeignPtrFinalizer finalizerFree fp
  addForeignPtrFinalizer (getFinalizer @t) fp  
#if TESTONLY  
  testonly_hsOwned p
#endif  
  pure $! coerce fp


{-# INLINE peekOutPtr #-}
peekOutPtr :: (Coercible (ForeignPtr t) t, HasFinalizer t) => Ptr (Ptr t) -> IO t
peekOutPtr pout = do
  pres <- peek pout
#if TESTONLY  
  testonly_hsOwned pres
#endif
  fmap coerce $ newForeignPtr getFinalizer pres

{-# INLINE peekOutNullablePtr #-}
peekOutNullablePtr :: (Coercible (ForeignPtr t) t, HasFinalizer t) => Ptr (Ptr t) -> IO (Maybe t)
peekOutNullablePtr pout = do
  pres <- peek pout
#if TESTONLY  
  testonly_hsOwned pres
#endif  
  if nullPtr == pres
    then pure Nothing
    else fmap (Just . coerce) $ newForeignPtr getFinalizer pres

_peekCOwnedOutPtr :: (Coercible (ForeignPtr t) t) => Ptr (Ptr t) -> IO t
_peekCOwnedOutPtr pout = do
  pres <- peek pout
  fmap coerce $ newForeignPtr_ pres

_peekCOwnedOutNullablePtr :: (Coercible (ForeignPtr t) t) => Ptr (Ptr t) -> IO (Maybe t)
_peekCOwnedOutNullablePtr pout = do
  pres <- peek pout
  if nullPtr == pres
    then pure Nothing
    else fmap (Just . coerce) $ newForeignPtr_ pres    

{-# INLINE peekCoerce #-}
peekCoerce :: (Coercible a b, Storable a) => Ptr a -> IO b
peekCoerce = fmap coerce peek

{-# INLINE allocWasmVal #-}
allocWasmVal :: (Ptr WasmVal -> IO a) -> IO a
allocWasmVal f = f =<< mallocBytes {#sizeof WasmVal #}

{-# INLINE allocWasmString #-}
allocWasmString :: (Ptr WasmString -> IO a) -> IO a
allocWasmString f = f =<< mallocBytes {#sizeof WasmEdge_String #}

-- _cOwnedWasmString :: WasmString -> IO ()
-- #if TESTONLY  
-- _cOwnedWasmString ws = withWasmString ws (testonly_cOwned)
-- #else
-- _cOwnedWasmString _ = pure ()
-- #endif

-- hsOwnedWasmString :: WasmString -> IO ()
-- hsOwnedWasmString ws = do
--   addForeignPtrFinalizer (getFinalizer @WasmString) (coerce ws)  
-- #if TESTONLY  
--   -- withForeignPtr (coerce ws) testonly_hsOwned
-- #endif
--   pure ()
  

{-# INLINE nullablePtrIn #-}
nullablePtrIn :: (Coercible t (ForeignPtr t)) => Maybe t -> (Ptr t -> IO r) -> IO r
nullablePtrIn Nothing f = f nullPtr
nullablePtrIn (Just t) f = withForeignPtr (coerce t) f

--

newtype ViaFromEnum t = ViaFromEnum {getHsEnumTy :: t}

instance Enum t => Storable (ViaFromEnum t) where
  sizeOf = sizeOf . fromEnum . getHsEnumTy
  alignment = alignment . fromEnum . getHsEnumTy
  peek = fmap (ViaFromEnum . toEnum) . peek @Int . castPtr 
  poke p v = poke @Int (castPtr p) (fromEnum $ getHsEnumTy v)
