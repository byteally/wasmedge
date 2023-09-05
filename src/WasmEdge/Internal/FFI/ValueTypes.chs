{-|
Module      : WasmEdge.Internal.FFI.ValueTypes
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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module WasmEdge.Internal.FFI.ValueTypes
  ( 
  --pointer
  HsRefPtr 
  ,WasmVal ( WasmInt32, WasmInt64, WasmFloat, WasmDouble, WasmInt128, WasmExternRef
           , WasmFuncRef, WasmNullExternRef, WasmNullFuncRef
           )
  ,WasmString
  ,WasmResult
  ,Limit
  ,ProgramOption
  ,ModuleDescriptor
  ,PluginVersionData
  ,PluginDescriptor
  ,ConfigureContext 
  ,StatisticsContext
  ,ASTModuleContext
  ,FunctionTypeContext
  ,MemoryTypeContext
  ,TableTypeContext
  ,GlobalTypeContext
  ,ImportTypeContext
  ,ExportTypeContext
  ,CompilerContext
  ,LoaderContext
  ,ValidatorContext
  ,ExecutorContext
  ,StoreContext
  ,ModuleInstanceContext
  ,FunctionInstanceContext
  ,TableInstanceContext
  ,MemoryInstanceContext
  ,GlobalInstanceContext
  ,CallingFrameContext
  ,Async
  ,VMContext
  ,PluginContext
  ,HostFuncT
  --funs
  ,valueGenI32
  ,valueGetI32
  ,valueGenI64
  ,valueGetI64
  ,valueGenF32
  ,valueGetF32
  ,valueGenF64
  ,valueGetF64
  ,valueGenV128
  ,valueGetV128
  ,valueGenNullRef
  ,valueIsNullRef
  ,mkStringFromBytesIO
  ,stringWrap
  ,wasmStringEq
  ,_stringCopy
  ,mkResultSuccess
  ,mkResultTerminate 
  ,mkResultFail
  ,resultOK
  ,resultGen 
  ,getResultCode
  ,getResultCategory
  ,getResultMessage
  ,valueGenFuncRef 
  ,valueGetFuncRef
  ,valueGenExternRef 
  ,valueGetExternRef 
  ,logSetErrorLevel 
  ,logSetDebugLevel
  ,configureCreate 
  ,configureAddProposal
  ,configureRemoveProposal
  ,configureHasProposal
  ,configureAddHostRegistration
  ,configureRemoveHostRegistration
  ,configureHasHostRegistration
  ,configureSetMaxMemoryPage
  ,configureGetMaxMemoryPage
  ,configureSetForceInterpreter
  ,configureIsForceInterpreter
  ,configureCompilerSetOptimizationLevel
  ,configureCompilerGetOptimizationLevel
  ,configureCompilerSetOutputFormat
  ,configureCompilerGetOutputFormat
  ,configureCompilerSetDumpIR
  ,configureCompilerIsDumpIR
  ,configureCompilerSetGenericBinary
  ,configureCompilerIsGenericBinary
  ,configureCompilerSetInterruptible
  ,configureCompilerIsInterruptible
  ,configureStatisticsSetInstructionCounting
  ,configureStatisticsIsInstructionCounting
  ,configureStatisticsSetCostMeasuring
  ,configureStatisticsIsCostMeasuring
  ,configureStatisticsSetTimeMeasuring
  ,configureStatisticsIsTimeMeasuring
  ,statisticsCreate
  ,statisticsGetInstrCount
  ,statisticsGetInstrPerSecond
  ,statisticsGetTotalCost
  ,statisticsSetCostTable
  ,statisticsSetCostLimit
  ,statisticsClear
  ,aSTModuleListImportsLength
  ,aSTModuleListExportsLength
  ,functionTypeCreate
  ,functionTypeGetParametersLength
  ,functionTypeGetParameters
  ,functionTypeGetReturnsLength 
  ,tableTypeCreate 
  ,tableTypeGetRefType
  ,memoryTypeCreate
  ,memoryTypeGetLimit
  ,globalTypeCreate
  ,globalTypeGetValType
  ,globalTypeGetMutability
  ,importTypeGetModuleName
  ,importTypeGetExternalName
  ,importTypeGetFunctionType
  ,importTypeGetTableType
  ,importTypeGetMemoryType
  ,importTypeGetGlobalType
  ,exportTypeGetExternalType
  ,exportTypeGetExternalName
  ,exportTypeGetFunctionType
  ,exportTypeGetTableType
  ,exportTypeGetMemoryType
  ,exportTypeGetGlobalType
  ,compilerCreate
  ,compilerCompile
  ,compilerCompileFromBuffer
  ,loaderCreate
  ,validatorCreate
  ,validatorValidate
  ,executorCreate
  ,executorInstantiate
  ,executorRegister
  ,executorRegisterImport
  ,executorInvoke
  ,executorAsyncInvoke
  ,functionInstanceCreate
  ,functionInstanceGetFunctionType 
  ,tableInstanceCreate
  ,tableInstanceGetTableType
  --,tableInstanceGetData
  --,tableInstanceSetData
  ,tableInstanceGetSize
  ,tableInstanceGrow
  ,memoryInstanceCreate
  ,memoryInstanceGetMemoryType
  ,memoryInstanceGetData
  ,memoryInstanceSetData
  ,memoryInstanceGetPointer
  ,memoryInstanceGetPointerConst
  ,memoryInstanceGetPageSize
  ,memoryInstanceGrowPage
  ,globalInstanceCreate
  ,globalInstanceGetGlobalType
  ,globalInstanceGetValue
  ,callingFrameGetExecutor
  ,callingFrameGetModuleInstance
  ,callingFrameGetMemoryInstance
  ,asyncWait
  ,asyncWaitFor
  ,asyncCancel
  ,asyncGetReturnsLength
  ,asyncGet
  ,vMCreate
  ,vMRegisterModuleFromFile
  ,vMRunWasmFromFile
  ,vMRunWasmFromBuffer
  ,vMRunWasmFromASTModule
  ,vMAsyncRunWasmFromFile
  ,vMAsyncRunWasmFromASTModule
  ,vMAsyncRunWasmFromBuffer
  ,vMRegisterModuleFromBuffer
  ,vMRegisterModuleFromASTModule
  ,vMRegisterModuleFromImport
  ,vMLoadWasmFromFile
  ,vMLoadWasmFromBuffer
  ,vMLoadWasmFromASTModule
  ,vMValidate
  ,vMInstantiate
  ,vMExecute
  ,vMExecuteRegistered
  ,vMAsyncExecute
  ,vMAsyncExecuteRegistered
  ,vMGetFunctionType
  ,vMGetFunctionTypeRegistered
  ,vMCleanup
  ,vMGetFunctionListLength
  ,vMGetFunctionList
  ,vMGetImportModuleContext
  ,vMGetActiveModule
  ,vMGetRegisteredModule
  ,vMListRegisteredModuleLength
  ,vMListRegisteredModule
  ,vMGetStoreContext
  ,vMGetLoaderContext
  ,vMGetValidatorContext
  ,vMGetExecutorContext
  ,vMGetStatisticsContext
  ,pluginLoadWithDefaultPaths 
  ,pluginLoadFromPath 
  ,pluginListPluginsLength 
  ,pluginListPlugins 
  ,pluginFind 
  ,pluginGetPluginName
  ,pluginListModuleLength 
  ,pluginListModule 
  ,pluginCreateModule 
  -- ,pluginGetDescriptor -- TODO: Getting undefined ref to 'WasmEdge_Plugin_GetDescriptor'
  --enum
  ,ProgramOptionType (..)
  ,Proposal  (..)
  ,HostRegistration (..)
  ,CompilerOptimizationLevel (..)
  ,CompilerOutputFormat (..)
  ,ErrCategory (..)
  ,ErrCode (..)
  ,ValType (..)
  ,NumType (..)
  ,RefType (..)
  ,Mutability (..)
  ,ExternalType (..)
  --Haskell funcs
  ,storeCreate 
  ,storeFindModule
  ,storeListModuleLength
  ,storeListModule
  ,moduleInstanceCreate
  ,moduleInstanceCreateWithData
  ,moduleInstanceCreateWASI
  ,moduleInstanceInitWASI
  ,moduleInstanceWASIGetExitCode
  ,moduleInstanceWASIGetNativeHandler
  ,moduleInstanceInitWasmEdgeProcess
  ,moduleInstanceGetModuleName
  ,moduleInstanceGetHostData
  ,moduleInstanceFindFunction
  ,moduleInstanceFindTable
  ,moduleInstanceFindMemory
  ,moduleInstanceFindGlobal
  ,moduleInstanceListFunctionLength
  ,moduleInstanceListFunction
  ,moduleInstanceListTableLength
  ,moduleInstanceListTable
  ,moduleInstanceListMemoryLength 
  ,moduleInstanceListMemory
  ,moduleInstanceListGlobalLength 
  ,moduleInstanceListGlobal
  ,moduleInstanceAddFunction 
  ,moduleInstanceAddTable
  ,moduleInstanceAddMemory 
  ,moduleInstanceAddGlobal
  ,tableTypeGetLimit
  ,hostFuncCallbackPure
  ,hostFuncCallback
  ,driverCompiler
  ,driverTool
  ,driverUniTool
  -- something
  ,fromHsRefIn
  ,fromHsRef
  ,toHsRef
  ,fromI128
  ,fromI128Alloc
  ,toI128
  ,stringCreateByCString
  ,mkStringFromBytes
  ,wrapCFinalizer
  ,finalize
  ,coercePtr
  ,useAsCStringLenBS
  ,useAsPtrCUCharLenBS
  ,_packCStringLenBS
  ,packCStringBS
  ,memBuffIn
  ,getValType
  ,allocMemBuff
  ,stringCopy
  ,wasmStringLength
  ,toText
  ,cToEnum
  ,cFromEnum
  ,fromStoreVecOr0Ptr
  ,fromStoreVecOr0PtrCULong
  ,fromVecOr0Ptr
  ,fromVecStringOr0Ptr
  ,fromMutIOVecOr0Ptr
  ,noFinalizer 
  ,peekOutPtr
  -- * Re-exports
  , Int128
  #if TESTONLY
  ,testonly_accquire
  , testonly_isAlive
  , testonly_release
#endif
  ) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Word
-- import Foreign
import Foreign.C
-- import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (Storable (..))
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
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
import Data.Typeable
#if TESTONLY
import Data.Unique
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.MVar
import GHC.Stack
#endif

#include "wasmedge/wasmedge.h"
#include <stdio.h>
#include <stdlib.h>

{#context prefix = "WasmEdge"#}

#if TESTONLY
testonly_ref_barrier :: MVar Unique
testonly_ref_barrier = unsafePerformIO newEmptyMVar
{-# NOINLINE testonly_ref_barrier #-}

testonly_ref_count :: MVar (Set Unique)
testonly_ref_count = unsafePerformIO $ newMVar mempty
{-# NOINLINE testonly_ref_count #-}

testonly_accquire :: HasCallStack => IO a -> IO (Unique, a)
testonly_accquire act = do
  uq <- newUnique
  putMVar testonly_ref_barrier uq
  !r <- act
  isUnlocked <- fmap (maybe True (uq/=)) $ tryReadMVar testonly_ref_barrier
  if isUnlocked
    then pure (uq, r)
    else error $ "No C finalizer added for resource accquired with ref: " ++ (show $ hashUnique uq)

testonly_release :: Unique -> IO ()
testonly_release uq = modifyMVar_ testonly_ref_count (pure . Set.delete uq)

testonly_isAlive :: Unique -> IO Bool
testonly_isAlive uq = withMVar testonly_ref_count (pure . Set.member uq)

testonly_ref :: IO (Maybe Unique)
testonly_ref = do
  modifyMVar testonly_ref_count $ \uqs -> do
    uqMay <- tryTakeMVar testonly_ref_barrier
    pure (maybe uqs (\uq -> Set.insert uq uqs) uqMay, uqMay)

foreign import ccall "wrapper" releaseFnPtr :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))
    

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

typedef struct U128 {
  uint64_t High;
  uint64_t Low;
} U128;

typedef struct WasmVal {
  U128 Val;
  enum WasmEdge_ValType Type;
} WasmVal;

typedef struct HsRef {
  U128 Fingerprint;
  void* Ref;
} HsRef;


uint128_t pack_uint128_t(U128 u128)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  if (0 == u128.High) {return (unsigned __int128)(u128.Low);}
  else { return (((unsigned __int128)(u128.High) << 64) + u128.Low); }
#else
  uint128_t r = {.Low = u128.Low, .High = u128.High};
  return r;
#endif
}

int128_t pack_int128_t(U128 u128)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  if (0 == u128.High) {return (__int128)(u128.Low);}
  else { return (((__int128)(u128.High) << 64) + u128.Low); }
#else
  int128_t r = {.Low = u128.Low, .High = u128.High};
  return r;
#endif
}

U128 unpack_int128_t(const int128_t i128)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  if ((__int128)0xFFFFFFFFFFFFFFFF < i128) {
    // TODO: Handle val greater than (maxBound :: Word64)
    U128 r = {.High = (uint64_t)((i128)>>64), .Low = (uint64_t)i128};
    return r;
  }
  else {
    U128 r = {.High = 0, .Low = (uint64_t)i128};
    return r;
  }    
#else
  U128 r = {.High = i128.High, .Low = i128.Low};
  return r;
#endif
}

U128 unpack_uint128_t(const uint128_t i128)
{
#if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  if ((unsigned __int128)0xFFFFFFFFFFFFFFFF < i128) {
    // TODO: Handle val greater than (maxBound :: Word64)
    U128 r = {.High = (uint64_t)(i128>>64), .Low = (uint64_t)i128};
    return r;
  }
  else {
    U128 r = {.High = 0, .Low = (uint64_t)i128};
    return r;
  }    
#else
  U128 r = {.High = i128.High, .Low = i128.Low};
  return r;
#endif
}

U128 WasmEdge_ValueToU128(WasmEdge_Value v)
{
  #if defined(__x86_64__) || defined(__aarch64__) ||                             \
    (defined(__riscv) && __riscv_xlen == 64)
  switch (v.Type){
    case WasmEdge_ValType_I32: case WasmEdge_ValType_I64: case WasmEdge_ValType_F32:
    case WasmEdge_ValType_F64:
         {U128 r = {.High = 0, .Low = (uint64_t)v.Value}; return r;}
    default:
      unpack_uint128_t(v.Value);
  }
#else
  U128 r = {.High = v.Value.High, .Low = v.Value.Low};
  return r; 
#endif
}

void ValueGenI32(WasmVal* valOut, const int32_t Val)
{ WasmEdge_Value r = WasmEdge_ValueGenI32(Val);
  valOut->Type = r.Type;
  valOut->Val = WasmEdge_ValueToU128(r);
}

int32_t ValueGetI32 (WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetI32(val);
}

void ValueGenI64(WasmVal* valOut, const int64_t Val)
{ WasmEdge_Value r = WasmEdge_ValueGenI64(Val);
  valOut->Type = r.Type;
  valOut->Val = WasmEdge_ValueToU128(r);
}

int64_t ValueGetI64 (WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetI64(val);
}

void ValueGenF32(WasmVal* valOut, const float Val)
{ WasmEdge_Value r = WasmEdge_ValueGenF32(Val);
  valOut->Type = r.Type;
  valOut->Val = WasmEdge_ValueToU128(r);
}

float ValueGetF32 (WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetF32(val);
}

void ValueGenF64(WasmVal* valOut, const double Val)
{ WasmEdge_Value r = WasmEdge_ValueGenF64(Val);
  valOut->Type = r.Type;
  valOut->Val = WasmEdge_ValueToU128(r);
}

double ValueGetF64 (WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetF64(val);
}

void ValueGenV128(WasmVal* valOut, const U128* Val)
{ WasmEdge_Value r = WasmEdge_ValueGenV128(pack_int128_t(*Val));
  valOut->Type = r.Type;
  valOut->Val = WasmEdge_ValueToU128(r);
}

void ValueGetV128 (WasmVal* v, U128* u128Out)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  *u128Out = unpack_int128_t(WasmEdge_ValueGetV128(val));
}

void ValueGenNullRef(WasmVal* valOut, const enum WasmEdge_RefType T)
{ WasmEdge_Value r = WasmEdge_ValueGenNullRef(T);
  valOut->Type = r.Type;
  valOut->Val = WasmEdge_ValueToU128(r);
}

bool ValueIsNullRef (WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueIsNullRef(val);
}

void ValueGenFuncRef(WasmVal* valOut, const WasmEdge_FunctionInstanceContext *Cxt)
{ WasmEdge_Value r = WasmEdge_ValueGenFuncRef(Cxt);
  valOut->Type = r.Type;
  valOut->Val = WasmEdge_ValueToU128(r);
}

const WasmEdge_FunctionInstanceContext * ValueGetFuncRef (WasmVal* v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_ValueGetFuncRef(val);
}

void ValueGenExternRef(WasmVal* valOut, HsRef *hsRef)
{ WasmEdge_Value r = WasmEdge_ValueGenExternRef(hsRef);
  valOut->Type = r.Type;
  valOut->Val = WasmEdge_ValueToU128(r);
}

const HsRef * ValueGetExternRef (WasmVal* v)
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
    *pRes = (WasmVal) {.Val = WasmEdge_ValueToU128(Returns[i]), .Type = Returns[i].Type};
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
    pOut->Type = r.Type;
    pOut->Val = WasmEdge_ValueToU128(r);
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
   WasmEdge_Value Data = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  *resOut = WasmEdge_TableInstanceGetData(Cxt,&Data,Offset);
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
    WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
    val = WasmEdge_GlobalInstanceGetValue(Cxt);
}
void GlobalInstanceSetValueOut(WasmEdge_GlobalInstanceContext *Cxt,const WasmVal *v)
{
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  WasmEdge_GlobalInstanceSetValue(Cxt,val);
}
void AsyncGetOut(WasmEdge_Result *resOut,const WasmEdge_Async *Cxt, WasmVal *v,const uint32_t ReturnLen)
{
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  *resOut = WasmEdge_AsyncGet(Cxt,&Returns,ReturnLen);
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
    *pRes = (WasmVal) {.Val = WasmEdge_ValueToU128(Returns[i]), .Type = Returns[i].Type};
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
    *pRes = (WasmVal) {.Val = WasmEdge_ValueToU128(Returns[i]), .Type = Returns[i].Type};
  }
  free(Params);
  free(Returns);
}

WasmEdge_Async *VMAsyncRunWasmFromFileOut(WasmEdge_VMContext *Cxt, const char *Path, const WasmEdge_String FuncName,const WasmVal **WValParams, const uint32_t ParamLen)
{
 WasmEdge_Value *Params = (WasmEdge_Value *)malloc(ParamLen * sizeof(WasmEdge_Value));
  for (int i = 0; i < ParamLen; i++)
  {
    const WasmVal* pVal = WValParams[i];
    Params[i] = (WasmEdge_Value) {.Value = pack_uint128_t(pVal->Val), .Type = pVal->Type};
  }
  WasmEdge_Async *res = WasmEdge_VMAsyncRunWasmFromFile(Cxt,Path,FuncName,Params,ParamLen);
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
    *pRes = (WasmVal) {.Val = WasmEdge_ValueToU128(Returns[i]), .Type = Returns[i].Type};
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
    *pRes = (WasmVal) {.Val = WasmEdge_ValueToU128(Returns[i]), .Type = Returns[i].Type};
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
    *pRes = (WasmVal) {.Val = WasmEdge_ValueToU128(Returns[i]), .Type = Returns[i].Type};
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

{-|
  HsRef
-}
{#pointer *HsRef as HsRefPtr foreign newtype #}
{#pointer *WasmVal as WasmVal foreign newtype #}
{-|
  WasmEdge string struct.
-}
{#pointer *WasmEdge_String as WasmString foreign finalizer StringDeleteByPtr as deleteString newtype #}
instance HasFinalizer WasmString where
  getFinalizer = deleteString

{#pointer *WasmEdge_Result as WasmResult foreign newtype #}
{#pointer *WasmEdge_Limit as Limit foreign newtype #}
-- | Program option for plugins.
{#pointer *WasmEdge_ProgramOption as ProgramOption foreign newtype #}
-- | Module descriptor for plugins.
{#pointer *WasmEdge_ModuleDescriptor as ModuleDescriptor foreign newtype #}
-- | Version data for plugins.
{#pointer *WasmEdge_PluginVersionData as PluginVersionData foreign newtype #}
-- | Plugin descriptor for plugins.
{#pointer *WasmEdge_PluginDescriptor as PluginDescriptor foreign newtype #}

---
fromHsRefIn :: HsRef -> (Ptr HsRefPtr -> IO a) -> IO a
fromHsRefIn = fromHsRefGenIn

fromHsRefAsVoidPtrIn :: HsRef -> (Ptr () -> IO a) -> IO a
fromHsRefAsVoidPtrIn = fromHsRefGenIn

fromHsRefGenIn :: HsRef -> (Ptr p -> IO a) -> IO a
fromHsRefGenIn (HsRef (Fingerprint hi lo) sp) f = do
  fp <- mallocForeignPtrBytes {#sizeof HsRef#} --mallocForeignPtr @HsRefPtr
  withForeignPtr fp $ \p -> do
    {#set HsRef.Fingerprint.High#} p (fromIntegral hi)
    {#set HsRef.Fingerprint.Low#} p (fromIntegral lo)
    {#set HsRef.Ref#} p (castStablePtrToPtr sp)
    f p

toHsRefOut :: Ptr HsRefPtr -> IO HsRef
toHsRefOut hsr = do
  hi <- {#get HsRef.Fingerprint.High#} hsr
  lo <- {#get HsRef.Fingerprint.Low#} hsr
  r <- {#get HsRef.Ref#} hsr
  pure $ HsRef (Fingerprint (fromIntegral hi) (fromIntegral lo)) (castPtrToStablePtr r)

toHsRefFromVoidPtrOut :: Ptr () -> IO HsRef
toHsRefFromVoidPtrOut = toHsRefOut . castPtr

fromHsRefWithFinalzrIn :: HsRef -> ((Ptr (), FunPtr (Ptr () -> IO ())) -> IO a) -> IO a
fromHsRefWithFinalzrIn hsRef f = do
  hsDataFinalzr <- finalizerHSData $ const (freeHsRef hsRef)
  fromHsRefAsVoidPtrIn hsRef $ \pRef -> f (pRef, hsDataFinalzr)

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

\param Val the reference of WasmEdge_Value struct where the I32 value would be returned
\param Val the I32 value.

\returns void
-}
{#fun pure unsafe ValueGenI32 as ^ {+, `Int32'} -> `WasmVal' #}
{-|
  Retrieve the I32 value from the WASM value.
 
  \param Val the WasmEdge_Value struct.
 
  \returns I32 value in the input struct.
-}
{#fun pure unsafe ValueGetI32 as ^ {`WasmVal'} -> `Int32' #}

{-|
  Generate the I64 WASM value.
 
  \params WasmEdge_Value 
  \param Val the I64 value.
 
  \returns WasmEdge_Value struct with the I64 value.
-}
{#fun pure unsafe ValueGenI64 as ^ {+, `Int64'} -> `WasmVal' #}
{-|
  Retrieve the I64 value from the WASM value.
 
  \param Val the WasmEdge_Value struct.
 
  \returns I64 value in the input struct.
WASMEDGE_CAPI_EXPORT extern int64_t
-}
{#fun pure unsafe ValueGetI64 as ^ {`WasmVal'} -> `Int64' #}

{-|
  Generate the F32 WASM value.
 
  \param Val the F32 value.
 
  \returns WasmEdge_Value struct with the F32 value.
-}
{#fun pure unsafe ValueGenF32 as ^ {+, `Float'} -> `WasmVal' #}
{-|
  Retrieve the F32 value from the WASM value.
 
  \param Val the WasmEdge_Value struct.
 
  \returns F32 value in the input struct.
-}
{#fun pure unsafe ValueGetF32 as ^ {`WasmVal'} -> `Float' #}

{-|
  Generate the F64 WASM value.
  \param refere                                   
  \param Val the F64 value.
 
  \returns WasmEdge_Value struct with the F64 value.
-}
{#fun pure unsafe ValueGenF64 as ^ {+, `Double'} -> `WasmVal' #}
{-|
  Retrieve the F64 value from the WASM value.
 
  \param Val the WasmEdge_Value struct.
 
  \returns F64 value in the input struct.
-}
{#fun pure unsafe ValueGetF64 as ^ {`WasmVal'} -> `Double' #}

{-|
  Generate the V128 WASM value.
 
  \param Val the V128 value.
 
  \returns WasmEdge_Value struct with the V128 value.
-}
{#fun pure unsafe ValueGenV128 as ^ {+, fromI128*`Int128'} -> `WasmVal' #}
{-|
  Retrieve the V128 value from the WASM value.
 
  \param Val the WasmEdge_Value struct.
 
  \returns V128 value in the input struct.
-}
{#fun pure unsafe ValueGetV128 as ^ {`WasmVal', fromI128Alloc-`Int128'toI128*} -> `()' #}

{-|
  Generate the NULL reference WASM value.
 
  The values generated by this function are only meaningful when the
  `WasmEdge_Proposal_BulkMemoryOperations` or the
  `WasmEdge_Proposal_ReferenceTypes` turns on in configuration.
  
  \param v The refernece to WasmEdge_Value, in which the result would be stored.
  \param T the reference type.
 
  \returns WasmEdge_Value struct with the NULL reference.
-}
{#fun pure unsafe ValueGenNullRef as ^ {+, cFromEnum`RefType'} -> `WasmVal' #}
{-|
  Specify the WASM value is a null reference or not.
 
  \param Val the WasmEdge_Value struct.
 
  \returns true if the value is a null reference, false if not.
-}
{#fun pure unsafe ValueIsNullRef as ^ {`WasmVal'} -> `Bool' #}

{-|
Creation of the WasmEdge_String with the C string.

The caller owns the object and should call `WasmEdge_StringDelete` to
destroy it. This function only supports the C string with NULL termination.
If the input string may have `\0` character, please use the
`WasmEdge_StringCreateByBuffer` instead.

\params strOut reference to WasmEdge_String in which the result would be stored.
\param Str the NULL-terminated C string to copy into the WasmEdge_String
object.

\returns string object. Length will be 0 and Buf will be NULL if failed or
the input string is a NULL.
-}
{#fun unsafe StringCreateByCStringOut as stringCreateByCString {+,`String'} -> `WasmString' #}
{#fun unsafe StringCreateByBufferOut as mkStringFromBytesIO {+, useAsCStringLenBS*`ByteString'& } -> `WasmString' #}
{#fun pure unsafe StringWrapOut as stringWrap {+, useAsCStringLenBS*`ByteString'&} -> `WasmString' #}

{-|
Compare the two WasmEdge_String objects.

\param Str1 the first WasmEdge_String object to compare.
\param Str2 the second WasmEdge_String object to compare.

\returns true if the content of two WasmEdge_String objects are the same,
false if not.
-}
{#fun pure unsafe WasmEdge_StringIsEqual as wasmStringEq {%`WasmString', %`WasmString'} -> `Bool' #}
{-|
  Copy the content of WasmEdge_String object to the buffer.
 
  This function copy at most `Len` characters from the `WasmEdge_String`
  object to the destination buffer. If the string length is less than `Len`
  characters long, the remainder of the buffer is filled with `\0' characters.
  Otherwise, the destination is not terminated.
 
  \param Str the source WasmEdge_String object to copy.
  \param Buf the buffer to fill the string content.
  \param Len the buffer length.
 
  \returns the copied length of string.
-}
{#fun pure unsafe WasmEdge_StringCopy as _stringCopy {%`WasmString', memBuffIn*`MemBuff'&} -> `Word32' #}
{#fun pure unsafe C_Result_Success as mkResultSuccess {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Terminate as mkResultTerminate {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Fail as mkResultFail {+} -> `WasmResult' #}

fromI128 :: Int128 -> (Ptr () -> IO a) -> IO a
fromI128 i128 f = do
  fp <- mallocForeignPtrBytes {#sizeof U128#}
  withForeignPtr fp $ \p -> do
    {#set U128.High#} p (fromIntegral $ int128Hi64 i128)
    {#set U128.Low#} p (fromIntegral $ int128Lo64 i128)
    f p

fromI128Alloc :: (Ptr () -> IO Int128) -> IO Int128
fromI128Alloc f = do
  fp <- mallocForeignPtrBytes {#sizeof U128#}
  withForeignPtr fp f

toI128 :: Ptr () -> IO Int128
toI128 p = do
  hi <- {#get U128.High#} p
  lo <- {#get U128.Low#} p
  pure $ Int128 {int128Hi64 = fromIntegral hi, int128Lo64 = fromIntegral lo}


{-|
  Converting ByteString to WasmEdge_String
-}
mkStringFromBytes :: ByteString -> WasmString
mkStringFromBytes bs = unsafePerformIO $ wrapCFinalizer deleteString $ mkStringFromBytesIO bs

wrapCFinalizer :: forall t.(Coercible t (ForeignPtr t)) => FinalizerPtr t -> IO t -> IO t
wrapCFinalizer final tAct = tAct >>= \t -> do
  addForeignPtrFinalizer final (coerce t)
#if TESTONLY
  uqMay <- testonly_ref
  case uqMay of
    Nothing -> pure ()
    Just _ -> pure ()
    -- Just uq -> do
    --   relFin <- releaseFnPtr @t (const $ testonly_release uq)
    --   addForeignPtrFinalizer relFin (coerce t)
#endif  
  pure t
{-#INLINE wrapCFinalizer #-}

class Coercible t (ForeignPtr t) => HasFinalizer t where
  runFinalizer :: t -> IO ()
  runFinalizer t = finalizeForeignPtr @t (coerce t)

  getFinalizer :: FinalizerPtr t
 
{-|
  Finalize
-}
finalize :: HasFinalizer t => t -> IO ()
finalize = runFinalizer

coercePtr :: Coercible a b => Ptr a -> Ptr b
coercePtr = castPtr

useAsCStringLenBS :: ByteString -> ((CString, CUInt) -> IO a) -> IO a
useAsCStringLenBS bs f = BS.useAsCStringLen bs (\strLen -> f (fromIntegral <$> strLen))

useAsPtrCUCharLenBS :: ByteString -> ((Ptr CUChar, CUInt) -> IO a) -> IO a
useAsPtrCUCharLenBS bs f = BS.useAsCStringLen bs (\strLen -> f (bimap convPtrCCharToPtrCUChar fromIntegral strLen))
  where
    convPtrCCharToPtrCUChar :: CString -> Ptr CUChar
    convPtrCCharToPtrCUChar = castPtr
    

_packCStringLenBS :: CString -> CUInt -> IO ByteString
_packCStringLenBS cstr len = BS.packCStringLen (cstr, fromIntegral len)

packCStringBS :: CString -> IO ByteString
packCStringBS cstr = BS.packCString cstr

memBuffIn :: MemBuff -> ((Ptr CChar, CUInt) -> IO a) -> IO a
memBuffIn mem f = withForeignPtr (memBuff mem) $ \p -> f (p, fromIntegral (memBuffLen mem))

data MemBuff = MemBuff {memBuffLen :: Int, memBuff :: ForeignPtr CChar}

allocMemBuff :: Int -> IO MemBuff
allocMemBuff sz = MemBuff sz <$> mallocForeignPtrBytes sz

{-|
  Copy the content of WasmEdge_String object to the buffer.
-}
stringCopy :: Word32 -> WasmString -> ByteString
stringCopy sz wstr = unsafePerformIO $ do
  mem <- allocMemBuff (fromIntegral sz)
  let cpLen = _stringCopy wstr mem
  withForeignPtr (memBuff mem) $ \p -> BS.packCStringLen (p, fromIntegral cpLen)

instance Eq WasmString where
  (==) = wasmStringEq

instance IsString WasmString where
  fromString = mkStringFromBytes . Char8.pack

instance Eq WasmResult where
  wr1 == wr2 = unsafePerformIO $ withWasmResult wr1 $ \wrp1 ->
    withWasmResult wr2 $ \wrp2 -> do
    r1 <- {#get WasmEdge_Result.Code #} wrp1
    r2 <- {#get WasmEdge_Result.Code #} wrp2
    pure $ r1 == r2
    
pattern WRSuccess :: WasmResult
pattern WRSuccess <- ((mkResultSuccess ==) -> True) where
  WRSuccess = mkResultSuccess

pattern WRTerminate :: WasmResult
pattern WRTerminate <- ((mkResultTerminate ==) -> True) where
  WRTerminate = mkResultTerminate

pattern WRFail :: WasmResult
pattern WRFail <- ((mkResultFail ==) -> True) where
  WRFail = mkResultFail

{-# COMPLETE WRSuccess, WRTerminate, WRFail #-}  

{- |
  Returning Length of WasmEdge_String 
-}
wasmStringLength :: WasmString -> IO Word32
wasmStringLength wstr = withWasmString wstr (fmap fromIntegral . {#get WasmEdge_String.Length #})

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

\param Res the WasmEdge_Result struct.

\returns true if the error code is WasmEdge_Result_Success or
WasmEdge_Result_Terminate, false for others.
-}
{#fun pure unsafe ResultOK as resultOK {%`WasmResult'} -> `Bool' #}

{-|
  Generate the result with code.
 
  \param Category the WasmEdge_ErrCategory to specify the error category.
  \param Code the 24-bit length error code. The data exceeds 24 bits will be
  stripped.
 
  \returns WasmEdge_Result struct with the given data.
-}
{#fun pure unsafe ResultGenOut as resultGen {+, cFromEnum`ErrCategory', `CUInt'} -> `WasmResult' #}
{-|
  Get the result code.
 
  \param Res the WasmEdge_Result struct.
 
  \returns result code (24-bit size data) in the WasmEdge_Result struct.
-}
{#fun pure unsafe WasmEdge_ResultGetCode as getResultCode {%`WasmResult'} -> `Word32' #}
{-|
  Get the error category.
 
  \param Res the WasmEdge_Result struct.
 
  \returns error category in the WasmEdge_Result struct.
-}
{#fun pure unsafe WasmEdge_ResultGetCategory as getResultCategory {%`WasmResult'} -> `ErrCategory'cToEnum #}
{-|
  Get the result message.
 
  The returned string must __NOT__ be destroyed.
  If the error category of the result is __NOT__ `WasmEdge_ErrCategory_WASM`,
  the message will always be "user defined error code".
 
  \param Res the WasmEdge_Result struct.
 
  \returns NULL-terminated C string of the corresponding error message.
-}
{#fun pure unsafe WasmEdge_ResultGetMessage as getResultMessage {%`WasmResult'} -> `ByteString'packCStringBS* #}
{#fun pure unsafe WasmEdge_LimitIsEqual as limitEq_ {%`Limit',%`Limit'} -> `Bool'#}

instance Eq Limit where
  (==) = limitEq_

{-|
  Opaque struct of WasmEdge configure.
-}
{#pointer *ConfigureContext as ^ foreign finalizer ConfigureDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *StatisticsContext as ^ foreign finalizer StatisticsDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ASTModuleContext as ^ foreign finalizer ASTModuleDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *FunctionTypeContext as ^ foreign finalizer FunctionTypeDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *MemoryTypeContext as ^ foreign finalizer MemoryTypeDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *TableTypeContext as ^ foreign finalizer TableTypeDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *GlobalTypeContext as ^ foreign finalizer GlobalTypeDelete as ^ newtype #}

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

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *LoaderContext as ^ foreign finalizer LoaderDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ValidatorContext as ^ foreign finalizer ValidatorDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ExecutorContext as ^ foreign finalizer ExecutorDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *StoreContext as ^ foreign finalizer StoreDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *ModuleInstanceContext as ^ foreign finalizer ModuleInstanceDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *FunctionInstanceContext as ^ foreign finalizer FunctionInstanceDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *TableInstanceContext as ^ foreign finalizer TableInstanceDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *MemoryInstanceContext as ^ foreign finalizer MemoryInstanceDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *GlobalInstanceContext as ^ foreign finalizer GlobalInstanceDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *CallingFrameContext as ^ foreign newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *Async as ^ foreign finalizer AsyncDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *VMContext as ^ foreign finalizer VMDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *PluginContext as ^ foreign newtype #}

{-|
  Generate the function reference WASM value.
 
  The values generated by this function are only meaningful when the
  `WasmEdge_Proposal_BulkMemoryOperations` or the
  `WasmEdge_Proposal_ReferenceTypes` turns on in configuration.
 
  \param Cxt the function instance context to convert to the reference.
 
  \returns WasmEdge_Value struct with the function reference.
-}
{#fun pure unsafe ValueGenFuncRef as ^ {+, `FunctionInstanceContext'} -> `WasmVal' #}
{-|
  Retrieve the function instance context from the WASM value.
 
  \param Val the WasmEdge_Value struct.
 
  \returns pointer to function instance context in the input struct.
-}
{#fun pure unsafe ValueGetFuncRef as ^ {`WasmVal'} -> `FunctionInstanceContext' #}
{-|
  Generate the function reference WASM value.
 
  The values generated by this function are only meaningful when the
  `WasmEdge_Proposal_ReferenceTypes` turns on in configuration.
 
  \param Ref the reference to the external object.
 
  \returns WasmEdge_Value struct with the external reference.
-}
{#fun pure unsafe ValueGenExternRef as ^ {+, fromHsRefIn*`HsRef'} -> `WasmVal' #}
{-|
  Retrieve the external reference from the WASM value.
 
  \param Val the WasmEdge_Value struct.
 
  \returns external reference in the input struct.
-}
{#fun pure unsafe ValueGetExternRef as ^ {`WasmVal'} -> `HsRef'toHsRefOut* #}

deriving newtype instance Storable ImportTypeContext
deriving newtype instance Storable ExportTypeContext

instance HasFinalizer ModuleInstanceContext where
  getFinalizer = moduleInstanceDelete

instance HasFinalizer FunctionTypeContext where
  getFinalizer = functionTypeDelete 

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
 
  The caller owns the object and should call `WasmEdge_ConfigureDelete` to
  destroy it.
 
  \returns pointer to the context, NULL if failed.
-}
{#fun unsafe ConfigureCreate as ^ {} -> `ConfigureContext'#}
{-|
  Add a proposal setting into the WasmEdge_ConfigureContext.
 
  For turning on a specific WASM proposal in VM, loader, or compiler contexts,
  etc., you can set the proposal value into the WasmEdge_ConfigureContext and
  create the VM, loader, or compiler contexts, etc. with this context.
 
  ```c
  WasmEdge_ConfigureContext *Conf = WasmEdge_ConfigureCreate();
  WasmEdge_ConfigureAddProposal(Conf, WasmEdge_Proposal_BulkMemoryOperations);
  WasmEdge_ConfigureAddProposal(Conf, WasmEdge_Proposal_ReferenceTypes);
  WasmEdge_ConfigureAddProposal(Conf, WasmEdge_Proposal_SIMD);
  WasmEdge_VMContext *VM = WasmEdge_VMCreate(Conf, NULL);
  ```
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to add the proposal value.
  \param Prop the proposal value.
-}
{#fun unsafe ConfigureAddProposal as ^ {`ConfigureContext',`Proposal'} -> `()'#}
{-|
  Remove a proposal setting in the WasmEdge_ConfigureContext.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to remove the proposal.
  \param Prop the proposal value.
-}
{#fun unsafe ConfigureRemoveProposal as ^ {`ConfigureContext',`Proposal'} -> `()'#}

{-|
  Check if a proposal setting exists in the WasmEdge_ConfigureContext or not.
  This function is thread-safe.
  \param Cxt the WasmEdge_ConfigureContext to check the proposal value.
  \param Prop the proposal value.
  \returns true if the proposal setting exists, false if not.
-}
{#fun unsafe ConfigureHasProposal as ^ {`ConfigureContext',`Proposal'} -> `Bool'#}

{- |
  Add a built-in host registration setting into WasmEdge_ConfigureContext.
 
  For turning on the Wasi support in `WasmEdge_VMContext`, you can set the
  built-in host registration value into the `WasmEdge_ConfigureContext` and
  create VM with this context.
 
  ```c
  WasmEdge_ConfigureContext *Conf = WasmEdge_ConfigureCreate();
  WasmEdge_ConfigureAddHostRegistration(Conf, WasmEdge_HostRegistration_Wasi);
  WasmEdge_VMContext *VM = WasmEdge_VMCreate(Conf, NULL);
  ```
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to add built-in host registration.
  \param Host the built-in host registration value.
-}
{#fun unsafe ConfigureAddHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `()'#}

{- |
  Remove a built-in host registration setting in the
  WasmEdge_ConfigureContext.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to remove the host
  pre-registration.
  \param Host the built-in host registration value.
-}
{#fun unsafe ConfigureRemoveHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `()'#}
{-|
  Check if a built-in host registration setting exists in the
  WasmEdge_ConfigureContext or not.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to check the host pre-registration.
  \param Host the built-in host registration value.
 
  \returns true if the built-in host registration setting exists, false if
  not.
-}
{#fun unsafe ConfigureHasHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `Bool'#}
{-|
  Set the page limit of memory instances.
 
  Limit the page count (64KiB per page) in memory instances.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the maximum page count.
  \param Page the maximum page count.
-}
{#fun unsafe ConfigureSetMaxMemoryPage as ^ {`ConfigureContext', `Word32'} -> `()'#}
{-|
  Get the setting of the page limit of memory instances.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the maximum page count
  setting.
 
  \returns the page count limitation value.
-}
{#fun unsafe ConfigureGetMaxMemoryPage as ^ {`ConfigureContext'} -> `Word32'#}
{-|
  Set the force interpreter mode execution option.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the boolean value.
  \param isForceInterpreter the boolean value to determine to forcibly run
  WASM in interpreter mode or not.
-}
{#fun unsafe ConfigureSetForceInterpreter as ^ {`ConfigureContext', `Bool'} -> `()'#}
{-|
  Get the force interpreter mode execution option.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the boolean value.
 
  \returns the boolean value to determine to forcibly run WASM in interpreter
  mode or not.
-}
{#fun unsafe ConfigureIsForceInterpreter as ^ {`ConfigureContext'} -> `Bool'#}
{-|
  Set the optimization level of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the optimization level.
  \param Level the AOT compiler optimization level.
-}
{#fun unsafe ConfigureCompilerSetOptimizationLevel as ^ {`ConfigureContext', `CompilerOptimizationLevel'} -> `()'#}
{-|
  Get the optimization level of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the optimization level.
 
  \returns the AOT compiler optimization level.
-}
{#fun unsafe ConfigureCompilerGetOptimizationLevel as ^ {`ConfigureContext'} -> `CompilerOptimizationLevel'#}
{-|
  Set the output binary format of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the output binary format.
  \param Format the AOT compiler output binary format.
-}
{#fun unsafe ConfigureCompilerSetOutputFormat as ^ {`ConfigureContext', `CompilerOutputFormat'} -> `()'#}
{-|
  Get the output binary format of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the output binary format.
 
  \returns the AOT compiler output binary format.
-}
{#fun unsafe ConfigureCompilerGetOutputFormat as ^ {`ConfigureContext'} -> `CompilerOutputFormat'#}
{-|
  Set the dump IR option of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the boolean value.
  \param IsDump the boolean value to determine to dump IR or not when
  compilation in AOT compiler.
-}
{#fun unsafe ConfigureCompilerSetDumpIR as ^ {`ConfigureContext', `Bool'} -> `()'#}
{-|
  Get the dump IR option of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the boolean value.
 
  \returns the boolean value to determine to dump IR or not when compilation
  in AOT compiler.
-}
{#fun unsafe ConfigureCompilerIsDumpIR as ^ {`ConfigureContext'} -> `Bool'#}
{-|
  Set the generic binary option of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the boolean value.
  \param IsGeneric the boolean value to determine to generate the generic
  binary or not when compilation in AOT compiler.
-}
{#fun unsafe ConfigureCompilerSetGenericBinary as ^ {`ConfigureContext', `Bool'} -> `()'#}
{-|
  Get the generic binary option of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the boolean value.
 
  \returns the boolean value to determine to generate the generic binary or
  not when compilation in AOT compiler.
-}
{#fun unsafe ConfigureCompilerIsGenericBinary as ^ {`ConfigureContext'} -> `Bool'#}
{-|
  Set the interruptible option of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the boolean value.
  \param IsInterruptible the boolean value to determine to generate
  interruptible binary or not when compilation in AOT compiler.
-}
{#fun unsafe ConfigureCompilerSetInterruptible as ^ {`ConfigureContext', `Bool'} -> `()'#}
{-|
  Get the interruptible option of the AOT compiler.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the boolean value.
 
  \returns the boolean value to determine to generate interruptible binary or
  not when compilation in AOT compiler.
-}
{#fun unsafe ConfigureCompilerIsInterruptible as ^ {`ConfigureContext'} -> `Bool'#}
{-|
  Set the instruction counting option for the statistics.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the boolean value.
  \param IsCount the boolean value to determine to support instruction
  counting when execution or not after compilation by the AOT compiler.
-}
{#fun unsafe ConfigureStatisticsSetInstructionCounting as ^ {`ConfigureContext', `Bool'} -> `()'#}
{-|
  Get the instruction counting option for the statistics.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the boolean value.
 
  \returns the boolean value to determine to support instruction counting when
  execution or not after compilation by the AOT compiler.
-}
{#fun unsafe ConfigureStatisticsIsInstructionCounting as ^ {`ConfigureContext'} -> `Bool'#}
{-|
  Set the cost measuring option for the statistics.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the boolean value.
  \param IsMeasure the boolean value to determine to support cost measuring
  when execution or not after compilation by the AOT compiler.
-}
{#fun unsafe ConfigureStatisticsSetCostMeasuring as ^ {`ConfigureContext', `Bool'} -> `()'#}
{-|
  Set the cost measuring option for the statistics.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the boolean value.
  \param IsMeasure the boolean value to determine to support cost measuring
  when execution or not after compilation by the AOT compiler.
-}
{#fun unsafe ConfigureStatisticsIsCostMeasuring as ^ {`ConfigureContext'} -> `Bool'#}
{-|
  Get the cost measuring option for the statistics.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to get the boolean value.
 
  \returns the boolean value to determine to support cost measuring when
  execution or not after compilation by the AOT compiler.
-}
{#fun unsafe ConfigureStatisticsSetTimeMeasuring as ^ {`ConfigureContext', `Bool'} -> `()'#}
{-|
  Set the time measuring option for the statistics.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ConfigureContext to set the boolean value.
  \param IsMeasure the boolean value to determine to support time when
  execution or not after compilation by the AOT compiler.
-}
{#fun unsafe ConfigureStatisticsIsTimeMeasuring as ^ {`ConfigureContext'} -> `Bool'#}

-- Statistics
{-|
  Creation of the WasmEdge_StatisticsContext.
 
  The caller owns the object and should call `WasmEdge_StatisticsDelete` to
  destroy it.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe StatisticsCreate as ^ {} -> `StatisticsContext'#}
{-|
  Get the instruction count in execution.
 
  \param Cxt the WasmEdge_StatisticsContext to get data.
 
  \returns the instruction count in total execution.
-}
{#fun unsafe StatisticsGetInstrCount as ^ {`StatisticsContext'} -> `Word64'#}
{-|
  Get the instruction count per second in execution.
 
  \param Cxt the WasmEdge_StatisticsContext to get data.
 
  \returns the instruction count per second.
-}
{#fun unsafe StatisticsGetInstrPerSecond as ^ {`StatisticsContext'} -> `Double'#}
{-|
  Get the total cost in execution.
 
  \param Cxt the WasmEdge_StatisticsContext to get data.
 
  \returns the total cost.
-}
{#fun unsafe StatisticsGetTotalCost as ^ {`StatisticsContext'} -> `Word64'#}
{-|
  Set the costs of instructions.
 
  \param Cxt the WasmEdge_StatisticsContext to set the cost table.
  \param CostArr the cost table array.
  \param Len the length of the cost table array.
-}
{#fun unsafe StatisticsSetCostTable as ^ {`StatisticsContext', fromStoreVecOr0Ptr*`Vector Word64'&} -> `()'#}
{-|
  Set the cost limit in execution.
 
  The WASM execution will be aborted if the instruction costs exceeded the
  limit and the ErrCode::Value::CostLimitExceeded will be returned.
 
  \param Cxt the WasmEdge_StatisticsContext to set the cost table.
  \param Limit the cost limit.
-}
{#fun unsafe StatisticsSetCostLimit as ^ {`StatisticsContext', `Word64'} -> `()'#}

{-|
  Clear all data in the WasmEdge_StatisticsContext.
 
  \param Cxt the WasmEdge_StatisticsContext to clear.
-}
{#fun unsafe StatisticsClear as ^ {`StatisticsContext'} -> `()'#}

-- AST Module
{-|
  Get the length of imports list of the AST module.
 
  \param Cxt the WasmEdge_ASTModuleContext.
 
  \returns length of the imports list.
-}
{#fun unsafe ASTModuleListImportsLength as ^ {`ASTModuleContext'} -> `Word32'#}
{#fun unsafe ASTModuleListImports as astModuleListImports_ {`ASTModuleContext', fromMutIOVecOr0Ptr*`IOVector ImportTypeContext'&} -> `Word32'#}
{-|
  Get the length of exports list of the AST module.
 
  \param Cxt the WasmEdge_ASTModuleContext.
 
  \returns length of the exports list.
-}
{#fun unsafe ASTModuleListExportsLength as ^ {`ASTModuleContext'} -> `Word32'#}
{#fun unsafe ASTModuleListExports as astModuleListExports_ {`ASTModuleContext', fromMutIOVecOr0Ptr*`IOVector ExportTypeContext'&} -> `Word32'#}

-- * Function
{-|
  Creation of the WasmEdge_FunctionTypeContext.
 
  The caller owns the object and should call `WasmEdge_FunctionTypeDelete` to
  destroy it.
 
  \param ParamList the value types list of parameters. NULL if the length is
  0.
  \param ParamLen the ParamList buffer length.
  \param ReturnList the value types list of returns. NULL if the length is 0.
  \param ReturnLen the ReturnList buffer length.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe FunctionTypeCreate as ^ {fromStoreVecOr0Ptr*`Vector ValType'&, fromStoreVecOr0Ptr*`Vector ValType'&} -> `FunctionTypeContext'#}
{-|
  Get the parameter types list length from the WasmEdge_FunctionTypeContext.
 
  \param Cxt the WasmEdge_FunctionTypeContext.
 
  \returns the parameter types list length.
-}
{#fun unsafe FunctionTypeGetParametersLength as ^ {`FunctionTypeContext'} -> `Word32'#}
{#fun unsafe FunctionTypeGetParameters as functionTypeGetParameters_ {`FunctionTypeContext', fromMutIOVecOfCEnumOr0Ptr*`IOVector ValType'&} -> `Word32'#}


{-|
Get the parameter types list from the WasmEdge_FunctionTypeContext.

If the `List` buffer length is smaller than the length of the parameter type
list, the overflowed values will be discarded.
\param Cxt the WasmEdge_FunctionTypeContext.
\param [out] List the WasmEdge_ValType buffer to fill the parameter value
types.
\param Len the value type buffer length.
\returns the actual parameter types list length.
-}
functionTypeGetParameters :: FunctionTypeContext -> Word32 -> IO (Vector ValType)
functionTypeGetParameters fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- functionTypeGetParameters_ fcxt v
  VS.unsafeFreeze $ VSM.slice 0 (fromIntegral len) v

astModuleListImports :: ASTModuleContext -> Word32 -> IO (Vector ImportTypeContext)
astModuleListImports fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- astModuleListImports_ fcxt v
  VS.unsafeFreeze $ VSM.unsafeCoerceMVector $ VSM.slice 0 (fromIntegral len) v

astModuleListExports :: ASTModuleContext -> Word32 -> IO (Vector ExportTypeContext)
astModuleListExports fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- astModuleListExports_ fcxt v
  VS.unsafeFreeze $ VSM.unsafeCoerceMVector $ VSM.slice 0 (fromIntegral len) v  
  

fromStoreVecOr0Ptr :: (Storable a, Num n) => Vector a -> ((Ptr n, CUInt) -> IO b) -> IO b
fromStoreVecOr0Ptr v f
  | VS.null v = f (nullPtr, 0)
  | otherwise = VS.unsafeWith v $ \p -> f (castPtr p, fromIntegral $ VS.length v)

fromStoreVecOr0PtrCULong :: (Storable a, Num n) => Vector a -> ((Ptr n, CULong) -> IO b) -> IO b
fromStoreVecOr0PtrCULong v f
  | VS.null v = f (nullPtr, 0)
  | otherwise = VS.unsafeWith v $ \p -> f (castPtr p, fromIntegral $ VS.length v)

fromVecOr0Ptr :: (Num sz) => (a -> IO (Ptr c)) -> V.Vector a -> ((Ptr (Ptr c), sz) -> IO b) -> IO b
fromVecOr0Ptr getPtr v f
  | V.null v = f (nullPtr, 0)
  | otherwise = do
      ptrs <- VSM.generateM (fromIntegral $ V.length v) (getPtr . V.unsafeIndex v)
      r <- fromMutIOVecOr0Ptr ptrs f
      VSM.mapM_ free ptrs
      pure r

fromVecStringOr0Ptr :: (Num sz) => V.Vector String -> ((Ptr (Ptr CChar), sz) -> IO b) -> IO b
fromVecStringOr0Ptr = fromVecOr0Ptr newCString

fromMutIOVecOr0Ptr :: (Storable a, Num sz) => IOVector a -> ((Ptr a, sz) -> IO b) -> IO b
fromMutIOVecOr0Ptr v f
  | VSM.null v = f (nullPtr, 0)
  | otherwise = VSM.unsafeWith v $ \p -> f (p, fromIntegral $ VSM.length v)

fromMutIOVecOfCEnumOr0Ptr :: (Storable a, Enum a) => IOVector a -> ((Ptr CInt, CUInt) -> IO b) -> IO b
fromMutIOVecOfCEnumOr0Ptr v f
  | VSM.null v = f (nullPtr, 0)
  | otherwise = VSM.unsafeWith v $ \p -> f (castPtr p, fromIntegral $ VSM.length v)

fromByteStringIn :: Coercible Word8 w8 => BS.ByteString -> ((Ptr w8, CUInt) -> IO b) -> IO b
fromByteStringIn bs f = UnsafeBS.unsafeUseAsCStringLen bs $ \(p, l) -> f (coercePtr (castPtr p :: Ptr Word8), fromIntegral l)
--

newtype ViaFromEnum t = ViaFromEnum {getHsEnumTy :: t}

instance Enum t => Storable (ViaFromEnum t) where
  sizeOf = sizeOf . fromEnum . getHsEnumTy
  alignment = alignment . fromEnum . getHsEnumTy
  peek = fmap (ViaFromEnum . toEnum) . peek @Int . castPtr 
  poke p v = poke @Int (castPtr p) (fromEnum $ getHsEnumTy v)
-- * Function Type
{-|
Get the return types list length from the WasmEdge_FunctionTypeContext.
\param Cxt the WasmEdge_FunctionTypeContext.
\returns the return types list length.
-}
{#fun unsafe FunctionTypeGetReturnsLength as ^ {`FunctionTypeContext'} -> `Word32'#}
{#fun unsafe FunctionTypeGetReturns as functionTypeGetReturns_ {`FunctionTypeContext', fromMutIOVecOfCEnumOr0Ptr*`IOVector ValType'&} -> `Word32'#}

functionTypeGetReturns :: FunctionTypeContext -> Word32 -> IO (Vector ValType)
functionTypeGetReturns fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- functionTypeGetReturns_ fcxt v
  VS.unsafeFreeze $ VSM.slice 0 (fromIntegral len) v

noFinalizer :: (Coercible (ForeignPtr t) t) => Ptr t -> IO t
noFinalizer = coerce . newForeignPtr_

-- Table Type
{-|
  Creation of the WasmEdge_TableTypeContext.
 
  The caller owns the object and should call `WasmEdge_TableTypeDelete` to
  destroy it.
 
  \param RefType the reference type of the table type.
  \param Limit the limit struct of the table type.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe TableTypeCreate as ^ {`RefType',%`Limit'} -> `TableTypeContext' #}
{-|
  Get the reference type from a table type.
  
  \param Cxt the WasmEdge_TableTypeContext.
  
  \returns the reference type of the table type.
-}
{#fun unsafe TableTypeGetRefType as ^ {`TableTypeContext'} -> `RefType'#}      
{-|
  Get the limit from a table type.
 
  \param Cxt the WasmEdge_TableTypeContext.
 
  \returns the limit struct of the table type.
-}
{#fun unsafe TableTypeGetLimitOut as tableTypeGetLimit {+,`TableTypeContext'} -> `Limit'#}
-- Memory Type

{- |
  Creation of the WasmEdge_MemoryTypeContext.
 
  The caller owns the object and should call `WasmEdge_MemoryTypeDelete` to
  destroy it.
 
  \param Limit the limit struct of the memory type.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe MemoryTypeCreate as ^ {%`Limit'} -> `MemoryTypeContext'#}  

{-|
  Get the limit from a memory type.
 
  \param limOut the reference to WasmEdge_Limit in which the returning result would be stored.
  \param Cxt the WasmEdge_MemoryTypeContext.
 
  \returns the limit struct of the memory type.
-}
{#fun unsafe MemoryTypeGetLimitOut as memoryTypeGetLimit {+,`MemoryTypeContext'} -> `Limit'#} 

-- Global Type
{-|
  Creation of the WasmEdge_GlobalTypeContext.
 
  The caller owns the object and should call `WasmEdge_GlobalTypeDelete` to
  destroy it.
 
  \param ValType the value type of the global type.
  \param Mut the mutation of the global type.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe GlobalTypeCreate as ^ {`ValType',`Mutability'} -> `GlobalTypeContext'#} 
{-|
Get the value type from a global type.

\param Cxt the WasmEdge_GlobalTypeContext.

\returns the value type of the global type.
-}
{#fun unsafe GlobalTypeGetValType as ^ {`GlobalTypeContext'} -> `ValType'#} 
{-
|
  Get the mutability from a global type.
 
  \param Cxt the WasmEdge_GlobalTypeContext.
 
  \returns the mutability of the global type.
-}
{#fun unsafe GlobalTypeGetMutability as ^ {`GlobalTypeContext'} -> `Mutability'#}

-- Import Type
{-|
  Get the module name from an import type.
 
  The returned string object is linked to the module name of the import type,
  and the caller should __NOT__ call the `WasmEdge_StringDelete`.
 
  \param strOut the reference to WasmEdge_String in which the result would be stored
  \param Cxt the WasmEdge_ImportTypeContext.
 
  \returns string object. Length will be 0 and Buf will be NULL if failed.
-}
{#fun unsafe ImportTypeGetModuleNameOut as importTypeGetModuleName {+,`ImportTypeContext'} -> `WasmString'#}

{- |
  Get the external name from an import type.
 
  The returned string object is linked to the external name of the import
  type, and the caller should __NOT__ call the `WasmEdge_StringDelete`.
 
  \param strOut the reference to WasmEdge_String in which the result would be stored
  \param Cxt the WasmEdge_ImportTypeContext.
 
  \returns string object. Length will be 0 and Buf will be NULL if failed.
-}
{#fun unsafe ImportTypeGetExternalNameOut as importTypeGetExternalName {+,`ImportTypeContext'} -> `WasmString'#}

{-|
  Get the external value (which is function type) from an import type.
 
  The import type context should be the one queried from the AST module
  context, or this function will cause unexpected error.
  The function type context links to the function type in the import type
  context and the AST module context. The caller should __NOT__ call the
  `WasmEdge_FunctionTypeDelete`.
 
  \param ASTCxt the WasmEdge_ASTModuleContext.
  \param Cxt the WasmEdge_ImportTypeContext which queried from the `ASTCxt`.
 
  \returns the function type. NULL if failed or the external type of the
  import type is not `WasmEdge_ExternalType_Function`.
-}
-- Question: Why have you written noFinalizer here
{#fun unsafe ImportTypeGetFunctionType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `FunctionTypeContext'noFinalizer*#} 
{-|
  Get the external value (which is table type) from an import type.
 
  The import type context should be the one queried from the AST module
  context, or this function will cause unexpected error.
  The table type context links to the table type in the import type context
  and the AST module context. The caller should __NOT__ call the
  `WasmEdge_TableTypeDelete`.
 
  \param ASTCxt the WasmEdge_ASTModuleContext.
  \param Cxt the WasmEdge_ImportTypeContext which queried from the `ASTCxt`.
 
  \returns the table type. NULL if failed or the external type of the import
  type is not `WasmEdge_ExternalType_Table`.
-}
{#fun unsafe ImportTypeGetTableType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `TableTypeContext'#} 
{-|
  Get the external value (which is memory type) from an import type.
 
  The import type context should be the one queried from the AST module
  context, or this function will cause unexpected error.
  The memory type context links to the memory type in the import type context
  and the AST module context. The caller should __NOT__ call the
  `WasmEdge_MemoryTypeDelete`.
 
  \param ASTCxt the WasmEdge_ASTModuleContext.
  \param Cxt the WasmEdge_ImportTypeContext which queried from the `ASTCxt`.
 
  \returns the memory type. NULL if failed or the external type of the import
  type is not `WasmEdge_ExternalType_Memory`.
-}
{#fun unsafe ImportTypeGetMemoryType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `MemoryTypeContext'#} 
{-|
  Get the external value (which is global type) from an import type.
 
  The import type context should be the one queried from the AST module
  context, or this function will cause unexpected error.
  The global type context links to the global type in the import type context
  and the AST module context. The caller should __NOT__ call the
  `WasmEdge_GlobalTypeDelete`.
 
  \param ASTCxt the WasmEdge_ASTModuleContext.
  \param Cxt the WasmEdge_ImportTypeContext which queried from the `ASTCxt`.
 
  \returns the global type. NULL if failed or the external type of the import
  type is not `WasmEdge_ExternalType_Global`.
-}
{#fun unsafe ImportTypeGetGlobalType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `GlobalTypeContext'#} 

-- Export Type
{-|
  Get the external type from an export type.
 
  \param Cxt the WasmEdge_ExportTypeContext.
 
  \returns the external type of the export type.
-}
{#fun unsafe ExportTypeGetExternalType as ^ {`ExportTypeContext'} -> `ExternalType'#} 
{-|
  Get the external name from an export type.
 
  The returned string object is linked to the external name of the export
  type, and the caller should __NOT__ call the `WasmEdge_StringDelete`.
 
  \param strOut Reference to WasmEdge_String in which the result would be stored.
  \param Cxt the WasmEdge_ExportTypeContext.
 
  \returns string object. Length will be 0 and Buf will be NULL if failed.
-}
{#fun unsafe ExportTypeGetExternalNameOut as exportTypeGetExternalName {+,`ExportTypeContext'} -> `WasmString'#} 
{-|
  Get the external value (which is function type) from an export type.
 
  The export type context should be the one queried from the AST module
  context, or this function will cause unexpected error.
  The function type context links to the function type in the export type
  context and the AST module context. The caller should __NOT__ call the
  `WasmEdge_FunctionTypeDelete`.
 
  \param ASTCxt the WasmEdge_ASTModuleContext.
  \param Cxt the WasmEdge_ExportTypeContext which queried from the `ASTCxt`.
 
  \returns the function type. NULL if failed or the external type of the
  export type is not `WasmEdge_ExternalType_Function`.
-}
{#fun unsafe ExportTypeGetFunctionType as ^ {`ASTModuleContext',`ExportTypeContext'} -> `FunctionTypeContext'#} 
{-|
  Get the external value (which is table type) from an export type.
 
  The export type context should be the one queried from the AST module
  context, or this function will cause unexpected error.
  The table type context links to the table type in the export type context
  and the AST module context. The caller should __NOT__ call the
  `WasmEdge_TableTypeDelete`.
 
  \param ASTCxt the WasmEdge_ASTModuleContext.
  \param Cxt the WasmEdge_ExportTypeContext which queried from the `ASTCxt`.
 
  \returns the table type. NULL if failed or the external type of the export
  type is not `WasmEdge_ExternalType_Table`.
-}
{#fun unsafe ExportTypeGetTableType as ^ {`ASTModuleContext',`ExportTypeContext'} -> `TableTypeContext'#}
{-|
  Get the external value (which is memory type) from an export type.
 
  The export type context should be the one queried from the AST module
  context, or this function will cause unexpected error.
  The memory type context links to the memory type in the export type context
  and the AST module context. The caller should __NOT__ call the
  `WasmEdge_MemoryTypeDelete`.
 
  \param ASTCxt the WasmEdge_ASTModuleContext.
  \param Cxt the WasmEdge_ExportTypeContext which queried from the `ASTCxt`.
 
  \returns the memory type. NULL if failed or the external type of the export
  type is not `WasmEdge_ExternalType_Memory`.
-}
{#fun unsafe ExportTypeGetMemoryType as ^ {`ASTModuleContext',`ExportTypeContext'} -> `MemoryTypeContext'#}
{-|
  Get the external value (which is global type) from an export type.
 
  The export type context should be the one queried from the AST module
  context, or this function will cause unexpected error.
  The global type context links to the global type in the export type context
  and the AST module context. The caller should __NOT__ call the
  `WasmEdge_GlobalTypeDelete`.
 
  \param ASTCxt the WasmEdge_ASTModuleContext.
  \param Cxt the WasmEdge_ExportTypeContext which queried from the `ASTCxt`.
 
  \returns the global type. NULL if failed or the external type of the export
  type is not `WasmEdge_ExternalType_Global`.
-}
{#fun unsafe ExportTypeGetGlobalType as ^ {`ASTModuleContext',`ExportTypeContext'} -> `GlobalTypeContext'#}

-- AOT Compiler
{-|
  Creation of the WasmEdge_CompilerContext.
 
  The caller owns the object and should call `WasmEdge_CompilerDelete` to
  delete it.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe CompilerCreate as ^ {`ConfigureContext'} -> `CompilerContext'#}

{- |
  Compile the input WASM from the file path.
 
  The compiler compiles the WASM from file path for the ahead-of-time mode and
  store the result to the output file path.
 
  \param resOut the reference to WasmEdge_Result in which the result would be stored
  \param Cxt the WasmEdge_CompilerContext.
  \param InPath the input WASM file path.
  \param OutPath the output WASM file path.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe CompilerCompileOut as compilerCompile {+,`CompilerContext',`String',`String'} -> `WasmResult'#} 

{-|
  Compile the input WASM from the given buffer.
 
  The compiler compiles the WASM from the given buffer for the
  ahead-of-time mode and store the result to the output file path.
 
  \param Cxt the WasmEdge_CompilerContext.
  \param InBuffer the input WASM binary buffer.
  \param InBufferLen the length of the input WASM binary buffer.
  \param OutPath the output WASM file path.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe CompilerCompileFromBufferOut as compilerCompileFromBuffer {+,`CompilerContext', fromStoreVecOr0PtrCULong*`Vector Word8'&,`String'} -> `WasmResult'#} 

-- Loader
{-|
  Creation of the WasmEdge_LoaderContext.
 
  The caller owns the object and should call `WasmEdge_LoaderDelete` to
  destroy it.
 
  \param ConfCxt the WasmEdge_ConfigureContext as the configuration of Loader.
  NULL for the default configuration.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe LoaderCreate as ^ {`ConfigureContext'} -> `LoaderContext'#}
{#fun unsafe LoaderParseFromFileOut as loaderParseFromFile_ {+,`LoaderContext',id`Ptr (Ptr ASTModuleContext)',`String'} -> `WasmResult'#}
{#fun unsafe LoaderParseFromBufferOut as loaderParseFromBuffer_ {+, `LoaderContext',id`Ptr (Ptr ASTModuleContext)',useAsPtrCUCharLenBS*`ByteString'&} -> `WasmResult'#}

-- Validator
{-|
  Creation of the WasmEdge_ValidatorContext.
 
  The caller owns the object and should call `WasmEdge_ValidatorDelete` to
  destroy it.
 
  \param ConfCxt the WasmEdge_ConfigureContext as the configuration of
  Validator. NULL for the default configuration.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe ValidatorCreate as ^ {`ConfigureContext'} -> `ValidatorContext'#}
{-|
  Validate the WasmEdge AST Module.
 
  \param resOut the reference to WasmEdge_Result in which the result would be stored
  \param Cxt the WasmEdge_ValidatorContext.
  \param ASTCxt the WasmEdge_ASTModuleContext to validate.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe ValidatorValidateOut as validatorValidate {+,`ValidatorContext',`ASTModuleContext'} -> `WasmResult'#}

-- Executor
{-|
  Creation of the WasmEdge_ExecutorContext.
 
  The caller owns the object and should call `WasmEdge_ExecutorDelete` to
  delete it.
 
  \param ConfCxt the WasmEdge_ConfigureContext as the configuration of
  Executor. NULL for the default configuration.
  \param StatCxt the WasmEdge_StatisticsContext as the statistics object set
  into Executor. The statistics will refer to this context, and the life cycle
  should be guaranteed until the executor context is deleted. NULL for not
  doing the statistics.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe ExecutorCreate as ^ {`ConfigureContext',`StatisticsContext'} -> `ExecutorContext'#}
{-|
  Instantiate an AST Module into a module instance.
 
  Instantiate an AST Module, and return an instantiated module instance
  context as the result. The caller owns the object and should call
  `WasmEdge_ModuleInstanceDelete` to destroy it. Developers can use the
  `WasmEdge_ModuleInstanceListFunction`,
  `WasmEdge_ModuleInstanceFindFunction`, etc. APIs to retrieve the exported
  instances from the result module instance.
 
  \param resOut the reference to WasmEdge_Result in which the result would be stored
  \param Cxt the WasmEdge_ExecutorContext to instantiate the module.
  \param [out] ModuleCxt the output WasmEdge_ModuleInstanceContext if
  succeeded.
  \param StoreCxt the WasmEdge_StoreContext to link the imports.
  \param ASTCxt the WasmEdge AST Module context generated by loader or
  compiler.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe ExecutorInstantiateOut as executorInstantiate {+,`ExecutorContext',alloca-`ModuleInstanceContext'peekOutPtr*,`StoreContext',`ASTModuleContext'} -> `WasmResult'#}
{-|
  Instantiate an AST Module into a named module instance and link into store.
 
  Instantiate an AST Module with the module name, return the instantiated
  module instance context as the result, and also register the module instance
  to the store. The caller owns the object and should call
  `WasmEdge_ModuleInstanceDelete` to destroy it.
  Developers can use the `WasmEdge_ModuleInstanceListFunction`,
  `WasmEdge_ModuleInstanceFindFunction`, etc. APIs to retrieve the exported
  instances from the result module instance.
  After calling this function, the output module instance will also be
  registered into the store, and the other modules can import the exported
  instances for linking when instantiation. Developers SHOULD guarantee the
  life cycle of this output module instance, or the error will occur when in
  execution after the module instance being destroyed if it has been imported
  by other modules. That is, developers have the responsibility to delete the
  output module instance even though the store being destroyed. When the
  module instance is deleted, it will be unregistered to the store
  automatically.
 
  \param resOut the reference to WasmEdge_Result in which the result would be stored
  \param Cxt the WasmEdge_ExecutorContext to instantiate the module.
  \param [out] ModuleCxt the output WasmEdge_ModuleInstanceContext if
  succeeded.
  \param StoreCxt the WasmEdge_StoreContext to link the imports.
  \param ASTCxt the WasmEdge AST Module context generated by loader or
  compiler.
  \param ModuleName the module name WasmEdge_String for all exported
  instances.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe ExecutorRegisterOut as executorRegister {+,`ExecutorContext',alloca-`ModuleInstanceContext'peekOutPtr*,`StoreContext',`ASTModuleContext',%`WasmString'} -> `WasmResult'#}
{-|
  Register a module instance into a store with exporting its module name.
 
  Register an existing module into the store with its module name.
  After calling this function, the existing module instance will be registered
  into the store, and the other modules can import the exported instances for
  linking when instantiation. Developers SHOULD guarantee the life cycle of
  this existing module instance, or the error will occur when in execution
  after the module instance being destroyed if it has been imported by other
  modules. When the module instance is deleted, it will be unregistered to the
  store automatically.
 
  \param resOut the reference to WasmEdge_Result in which the result would be stored
  \param Cxt the WasmEdge_ExecutorContext to instantiate the module.
  \param StoreCxt the WasmEdge_StoreContext to store the instantiated module.
  \param ImportCxt the WasmEdge_ModuleInstanceContext to register.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe ExecutorRegisterImportOut as executorRegisterImport {+,`ExecutorContext',`StoreContext',`ModuleInstanceContext'} -> `WasmResult'#}
{-|
  Invoke a WASM function by the function instance.
 
  After instantiating a WASM module, developers can get the function instance
  context from the module instance. Then developers can invoke the function
  through this API.
  \param resOut the reference to WasmEdge_Result in which the result would be stored
  \param Cxt the WasmEdge_ExecutorContext.
  \param FuncCxt the function instance context to invoke.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
  \param [out] Returns the WasmEdge_Value buffer to fill the return values.
  \param ReturnLen the return buffer length.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
executorInvoke ::
  ExecutorContext -- ^ the WasmEdge_ExecutorContext.
  -> FunctionInstanceContext -- ^ the function instance context to invoke.
  -> V.Vector WasmVal -- ^ the WasmEdge_Value buffer with the parameter values.
  -> IO (WasmResult, V.Vector WasmVal) -- ^ the WasmEdge_Value buffer to fill the return values.
executorInvoke ecxt ficxt pars = do
  funcType <- functionInstanceGetFunctionType ficxt
  parLen <- fromIntegral <$> functionTypeGetParametersLength funcType
  retLen <- fromIntegral <$> functionTypeGetReturnsLength funcType
  if parLen /= V.length pars
    then pure (WRFail, V.empty)
    else do
    paramsVSM <- VSM.generateM (parLen) ((flip withWasmVal pure) . (pars V.!))
    retsVSM <- VSM.new (retLen)
    wres <- executorInvoke_ ecxt ficxt paramsVSM retsVSM
    rets <- V.generateM (retLen) ((noFinalizer =<<) . (VSM.read retsVSM))
    pure (wres, rets)

{#fun unsafe ExecutorInvokeOut as executorInvoke_ {+,`ExecutorContext',`FunctionInstanceContext',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `WasmResult'#}
{-|
  Asynchronous invoke a WASM function by the function instance.
 
  After instantiating a WASM module, developers can get the function instance
  context from the module instance. Then developers can invoke the function
  asynchronously through this API.
 
  \param Cxt the WasmEdge_ExecutorContext.
  \param FuncCxt the function instance context to invoke.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
 
  \returns WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call
  `WasmEdge_AsyncDelete` to destroy this object.
-}
{#fun unsafe ExecutorAsyncInvokeOut as executorAsyncInvoke {`ExecutorContext',`FunctionInstanceContext',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `Async'#}

peekOutPtr :: (Coercible (ForeignPtr t) t, HasFinalizer t) => Ptr (Ptr t) -> IO t
peekOutPtr pout = do
  pres <- peek pout
  fmap coerce $ newForeignPtr getFinalizer pres

peekCoerce :: (Coercible a b, Storable a) => Ptr a -> IO b
peekCoerce = fmap coerce peek

-- Store
{-|
  Creation of the WasmEdge_StoreContext.
 
  The caller owns the object and should call `WasmEdge_StoreDelete` to destroy
  it.
  The store is the linker for multiple WASM module instances. The store will
  not own any module instance registered into it, and the module instances
  will automatically be unregistered if they are destroyed.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe StoreCreate as ^ {} -> `StoreContext'#} 
{-|
  Get the module instance context by the module name.
 
  After registering a WASM module, developers can call this function to find
  and get the registered module instance context by the module name.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_StoreContext.
  \param Name the module name WasmEdge_String.
 
  \returns pointer to the module instance context. NULL if not found.
-}
{#fun unsafe StoreFindModule as ^ {`StoreContext',%`WasmString'} -> `ModuleInstanceContext'#}
{-|
  Get the length of registered module list in store.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_StoreContext.
 
  \returns length of registered named module list.
-}
{#fun unsafe StoreListModuleLength as ^ {`StoreContext'} -> `Word32'#}
{-|
  List the registered module names.
 
  This function will list all registered module names.
  The returned module names filled into the `Names` array are linked to the
  registered module names in the store context, and the caller should __NOT__
  call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the registered
  named module list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_StoreContext.
  \param [out] Names the output names WasmEdge_String buffer of named modules.
  \param Len the buffer length.
 
  \returns actual registered named module list size.
-}
{#fun unsafe StoreListModuleOut as storeListModule {`StoreContext',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&} -> `Word32'#}

-- Module Instance
{-|
  Creation of the WasmEdge_ModuleInstanceContext.
 
  Create a module instance context with exported module name for host
  instances. Developer can use this API to create a module instance for
  collecting host functions, tables, memories, and globals.
  The caller owns the object and should call `WasmEdge_ModuleInstanceDelete`
  to destroy it.
 
  \param ModuleName the module name WasmEdge_String of this host module to
  import.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe ModuleInstanceCreate as ^ {%`WasmString'} -> `ModuleInstanceContext'#} 
{-|
  Creation of the WasmEdge_ModuleInstanceContext with host data.
 
  Create a module instance context with exported module name, host data, and
  host data finalizer for host instances. Developer can use this API to create
  a module instance for collecting host functions, tables, memories, and
  globals. When this created module instance being destroyed, the host data
  finalizer will be invoked. The caller owns the object and should call
  `WasmEdge_ModuleInstanceDelete` to destroy it.
 
  \param ModuleName the module name WasmEdge_String of this host module to
  import.
  \param HostData the host data to set into the module instance. When calling
  the finalizer, this pointer will become the argument of the finalizer
  function.
  \param Finalizer the function to finalize the host data.
 
  \returns pointer to context, NULL if failed.
-}
{#fun ModuleInstanceCreateWithData as ^ {%`WasmString',fromHsRefWithFinalzrIn*`HsRef'&} -> `ModuleInstanceContext'#}
{-|
  Creation of the WasmEdge_ModuleInstanceContext for the WASI specification.
 
  This function will create a WASI host module that contains the WASI host
  functions and initialize it. The caller owns the object and should call
  `WasmEdge_ModuleInstanceDelete` to destroy it.
 
  \param Args the command line arguments. The first argument suggests being
  the program name. NULL if the length is 0.
  \param ArgLen the length of the command line arguments.
  \param Envs the environment variables in the format `ENV=VALUE`. NULL if the
  length is 0.
  \param EnvLen the length of the environment variables.
  \param Preopens the directory paths to preopen. String format in
  `PATH1:PATH2` means the path mapping, or the same path will be mapped. NULL
  if the length is 0.
  \param PreopenLen the length of the directory paths to preopen.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe ModuleInstanceCreateWASI as ^ {fromVecStringOr0Ptr*`V.Vector String'&,fromVecStringOr0Ptr*`V.Vector String'&,fromVecStringOr0Ptr*`V.Vector String'&} -> `ModuleInstanceContext'#}
{-|
  Initialize the WasmEdge_ModuleInstanceContext for the WASI specification.
 
  This function will initialize the WASI host module with the parameters.
 
  \param Cxt the WasmEdge_ModuleInstanceContext of WASI import object.
  \param Args the command line arguments. The first argument suggests being
  the program name. NULL if the length is 0.
  \param ArgLen the length of the command line arguments.
  \param Envs the environment variables in the format `ENV=VALUE`. NULL if the
  length is 0.
  \param EnvLen the length of the environment variables.
  \param Preopens the directory paths to preopen. String format in
  `PATH1:PATH2` means the path mapping, or the same path will be mapped. NULL
  if the length is 0.
  \param PreopenLen the length of the directory paths to preopen.
-}
{#fun unsafe ModuleInstanceInitWASI as ^ {`ModuleInstanceContext',fromVecStringOr0Ptr*`V.Vector String'&,fromVecStringOr0Ptr*`V.Vector String'&,fromVecStringOr0Ptr*`V.Vector String'&} -> `()'#}
{-|
  Get the WASI exit code.
 
  This function will return the exit code after running the "_start" function
  of a `wasm32-wasi` program.
 
  \param Cxt the WasmEdge_ModuleInstanceContext of WASI import object.
 
  \returns the exit code after executing the "_start" function. Return
  `EXIT_FAILURE` if the `Cxt` is NULL or not a WASI host module.
-}
{#fun unsafe ModuleInstanceWASIGetExitCode as ^ {`ModuleInstanceContext'} -> `Word32'#}
{-|
  Get the native handler from the WASI mapped FD/Handler.
 
  This function will return the raw FD/Handler from a given mapped Fd
  or Handler.
 
  \param Cxt the WasmEdge_ModuleInstanceContext of WASI import object.
  \param Fd the WASI mapped Fd.
  \param [out] NativeHandler the raw Fd/Handler.
 
  \returns the error code. Return `0` if the Native Handler is found.
  Return `1` if the `Cxt` is `NULL`.
  Return `2` if the given mapped Fd/handler is not found.
-}
{#fun unsafe ModuleInstanceWASIGetNativeHandler as ^ {`ModuleInstanceContext',`Word32',alloca-`Word64'peekCoerce*} -> `Word32'#}
{-|
  Initialize the WasmEdge_ModuleInstanceContext for the wasmedge_process
  specification.
 
  This function will initialize the wasmedge_process host module with the
  parameters.
 
  \param AllowedCmds the allowed commands white list. NULL if the
  length is 0.
  \param CmdsLen the length of the allowed commands white list.
  \param AllowAll the boolean value to allow all commands. `false` is
  suggested. If this value is `true`, the allowed commands white list will not
  be recorded and all commands can be executed by wasmedge_process.
-}
{#fun unsafe ModuleInstanceInitWasmEdgeProcess as ^ {fromVecStringOr0Ptr*`V.Vector String'&,`Bool'} -> `()'#}
{-|
  Get the export module name of a module instance.
 
  The returned string object is linked to the module name of the module
  instance, and the caller should __NOT__ call the `WasmEdge_StringDelete`.

  \param strOut the reference to WasmEdge_String in which the result would be stored
  \param Cxt the WasmEdge_ModuleInstanceContext.
 
  \returns string object. Length will be 0 and Buf will be NULL if failed.
-}
{#fun unsafe ModuleInstanceGetModuleNameOut as moduleInstanceGetModuleName {+,`ModuleInstanceContext'} -> `WasmString'#}
{-|
  Get the host data set into the module instance when creating.
 
  The returned data is owned by the module instance, and will be passed into
  the finalizer when deleting this module instance.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
 
  \returns host data. NULL if the module instance context is NULL or no host
  data set into the module instance.
-}
{#fun unsafe ModuleInstanceGetHostData as ^ {`ModuleInstanceContext'} -> `HsRef'toHsRefFromVoidPtrOut*#}
{-|
  Get the exported function instance context of a module instance.
 
  The result function instance context links to the function instance in the
  module instance context and owned by the module instance context, and the
  caller should __NOT__ call the `WasmEdge_FunctionInstanceDelete`.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
  \param Name the function name WasmEdge_String.
 
  \returns pointer to the function instance context. NULL if not found.
-}
{#fun unsafe ModuleInstanceFindFunction as ^ {`ModuleInstanceContext',%`WasmString'} -> `FunctionInstanceContext'#}
{-|
  Get the exported table instance context of a module instance.
 
  The result table instance context links to the table instance in the module
  instance context and owned by the module instance context, and the caller
  should __NOT__ call the `WasmEdge_TableInstanceDelete`.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
  \param Name the table name WasmEdge_String.
 
  \returns pointer to the table instance context. NULL if not found.
-}
{#fun unsafe ModuleInstanceFindTable as ^ {`ModuleInstanceContext',%`WasmString'} -> `TableInstanceContext'#}
{-|
  Get the exported memory instance context of a module instance.
 
  The result memory instance context links to the memory instance in the
  module instance context and owned by the module instance context, and the
  caller should __NOT__ call the `WasmEdge_MemoryInstanceDelete`.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
  \param Name the memory name WasmEdge_String.
 
  \returns pointer to the memory instance context. NULL if not found.
-}
{#fun unsafe ModuleInstanceFindMemory as ^ {`ModuleInstanceContext',%`WasmString'} -> `MemoryInstanceContext'#}
{-|
  Get the exported global instance context of a module instance.
 
  The result global instance context links to the global instance in the
  module instance context and owned by the module instance context, and the
  caller should __NOT__ call the `WasmEdge_GlobalInstanceDelete`.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
  \param Name the global name WasmEdge_String.
 
  \returns pointer to the global instance context. NULL if not found.
-}
{#fun unsafe ModuleInstanceFindGlobal as ^ {`ModuleInstanceContext',%`WasmString'} -> `GlobalInstanceContext'#}
{-|
  Get the length of exported function list of a module instance.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
 
  \returns length of the exported function list.
-}
{#fun unsafe ModuleInstanceListFunctionLength as ^ {`ModuleInstanceContext'} -> `Word32'#}
{-|
  List the exported function names of a module instance.
 
  The returned function names filled into the `Names` array are linked to the
  exported names of functions of the module instance context, and the caller
  should __NOT__ call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the exported
  function list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
  \param [out] Names the output WasmEdge_String buffer of the function names.
  \param Len the buffer length.
 
  \returns actual exported function list size.
-}
{#fun unsafe ModuleInstanceListFunctionOut as moduleInstanceListFunction {`ModuleInstanceContext', fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&} -> `Word32'#}
{-|
  Get the length of exported table list of a module instance.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
 
  \returns length of the exported table list.
-}
{#fun unsafe ModuleInstanceListTableLength as ^ {`ModuleInstanceContext'} -> `Word32'#}
{-|
  List the exported table names of a module instance.
 
  The returned table names filled into the `Names` array are linked to the
  exported names of tables of the module instance context, and the caller
  should __NOT__ call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the exported
  table list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
  \param [out] Names the output WasmEdge_String buffer of the table names.
  \param Len the buffer length.
 
  \returns actual exported table list size.
-}
{#fun unsafe ModuleInstanceListTableOut as moduleInstanceListTable {`ModuleInstanceContext', fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&} -> `Word32'#}
{-|
  Get the length of exported memory list of a module instance.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
 
  \returns length of the exported memory list.
-}
{#fun unsafe ModuleInstanceListMemoryLength as ^ {`ModuleInstanceContext'} -> `Word32'#} 
{-|
  List the exported memory names of a module instance.
 
  The returned memory names filled into the `Names` array are linked to the
  exported names of memories of the module instance context, and the caller
  should __NOT__ call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the exported
  memory list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
  \param [out] Names the output WasmEdge_String buffer of the memory names.
  \param Len the buffer length.
 
  \returns actual exported memory list size.
-}
{#fun unsafe ModuleInstanceListMemoryOut as moduleInstanceListMemory {`ModuleInstanceContext', fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&} -> `Word32'#} 
{-|
  Get the length of exported global list of a module instance.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
 
  \returns length of the exported global list.
-}
{#fun unsafe ModuleInstanceListGlobalLength as ^ {`ModuleInstanceContext'} -> `Word32'#} 
{-|
  List the exported global names of a module instance.
 
  The returned global names filled into the `Names` array are linked to the
  exported names of globals of the module instance context, and the caller
  should __NOT__ call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the exported
  global list size, the overflowed return values will be discarded.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext.
  \param [out] Names the output WasmEdge_String buffer of the global names.
  \param Len the buffer length.
 
  \returns actual exported global list size.
-}
{#fun unsafe ModuleInstanceListGlobalOut as moduleInstanceListGlobal {`ModuleInstanceContext', fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&} -> `Word32'#} 
{-|
  Add a function instance context into a WasmEdge_ModuleInstanceContext.
 
  Export and move the ownership of the function instance into the module
  instance. The caller should __NOT__ access or destroy the function instance
  context after calling this function.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext to add the function instance.
  \param Name the export function name WasmEdge_String.
  \param FuncCxt the WasmEdge_FunctionInstanceContext to add.
-}
{#fun unsafe ModuleInstanceAddFunction as ^ {`ModuleInstanceContext',%`WasmString',`FunctionInstanceContext'} -> `()'#}
{-|
  Add a table instance context into a WasmEdge_ModuleInstanceContext.
 
  Export and move the ownership of the table instance into the module
  instance. The caller should __NOT__ access or destroy the table instance
  context after calling this function.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext to add the table instance.
  \param Name the export table name WasmEdge_String.
  \param TableCxt the WasmEdge_TableInstanceContext to add.
-}
{#fun unsafe ModuleInstanceAddTable as ^ {`ModuleInstanceContext',%`WasmString',`TableInstanceContext'} -> `()'#}
{-|
  Add a memory instance context into a WasmEdge_ModuleInstanceContext.
 
  Export and move the ownership of the memory instance into the module
  instance. The caller should __NOT__ access or destroy the memory instance
  context after calling this function.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext to add the memory instance.
  \param Name the export memory name WasmEdge_String.
  \param MemoryCxt the WasmEdge_MemoryInstanceContext to add.
-}
{#fun unsafe ModuleInstanceAddMemory as ^ {`ModuleInstanceContext',%`WasmString',`MemoryInstanceContext'} -> `()'#}
{-|
  Add a global instance context into a WasmEdge_ModuleInstanceContext.
 
  Export and move the ownership of the global instance into the module
  instance. The caller should __NOT__ access or destroy the global instance
  context after calling this function.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_ModuleInstanceContext to add the global instance.
  \param Name the export global name WasmEdge_String.
  \param GlobalCxt the WasmEdge_GlobalInstanceContext to add.
-}
{#fun unsafe ModuleInstanceAddGlobal as ^ {`ModuleInstanceContext',%`WasmString',`GlobalInstanceContext'} -> `()'#}

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
 
  The caller owns the object and should call `WasmEdge_FunctionInstanceDelete`
  to destroy it if the returned object is not added into a
  `WasmEdge_ModuleInstanceContext`. The following is an example to create a
  host function context.
  ```c
-}
{#fun FunctionInstanceCreateBndr as functionInstanceCreate {`FunctionTypeContext',`HostFuncT',fromHsRefIn*`HsRef',`Word64'} -> `FunctionInstanceContext'#}
-- TODO:
--{#fun unsafe FunctionInstanceCreateBinding as ^ {`FunctionTypeContext',`WrapFuncT',fromHsRefAsVoidPtrIn*`HsRef',fromHsRefAsVoidPtrIn*`HsRef',`Word64'} -> `FunctionInstanceContext'#}

{-|
  Get the function type context of the function instance.
 
  The function type context links to the function type in the function
  instance context and owned by the context. The caller should __NOT__ call
  the `WasmEdge_FunctionTypeDelete`.
 
  \param Cxt the WasmEdge_FunctionInstanceContext.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe FunctionInstanceGetFunctionType as ^ {`FunctionInstanceContext'} -> `FunctionTypeContext'#}

-- Table Instance
{-|
  Creation of the WasmEdge_TableInstanceContext.
 
  The caller owns the object and should call `WasmEdge_TableInstanceDelete` to
  destroy it if the returned object is not added into a
  `WasmEdge_ModuleInstanceContext`.
 
  \param TabType the table type context to initialize the table instance
  context.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe TableInstanceCreate as ^ {`TableTypeContext'} -> `TableInstanceContext'#}
{-|
  Get the table type context from a table instance.
 
  The table type context links to the table type in the table instance context
  and owned by the context. The caller should __NOT__ call the
  `WasmEdge_TableTypeDelete`.
 
  \param Cxt the WasmEdge_TableInstanceContext.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe TableInstanceGetTableType as ^ {`TableInstanceContext'} -> `TableTypeContext'#}
{-|
  Get the reference value in a table instance.
 
  \param Cxt the WasmEdge_TableInstanceContext.
  \param [out] Data the result reference value.
  \param Offset the reference value offset (index) in the table instance.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
-- TODO: Simialar to MemoryInstanceSetData but Length is not given
-- {#fun unsafe TableInstanceGetDataOut as tableInstanceGetData  {+,`TableInstanceContext',fromByteStringIn*`ByteString'&} -> `WasmResult'#}
{-|
  Set the reference value into a table instance.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_TableInstanceContext.
  \param Data the reference value to set into the table instance.
  \param Offset the reference value offset (index) in the table instance.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
-- {#fun unsafe TableInstanceSetDataOut as tableInstanceSetData  {+,`TableInstanceContext',fromByteStringIn*`ByteString'&} -> `WasmResult'#}
{-|
  Get the size of a table instance.
 
  \param Cxt the WasmEdge_TableInstanceContext.
 
  \returns the size of the table instance.
-}
{#fun unsafe TableInstanceGetSize as ^ {`TableInstanceContext'} -> `Word32'#} 
{-|
  Grow a table instance with a size.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_TableInstanceContext.
  \param Size the count of reference values to grow in the table instance.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe TableInstanceGrowOut as tableInstanceGrow {+,`TableInstanceContext',`Word32'} -> `WasmResult'#}

-- Memory Instance
{-|
  Creation of the WasmEdge_MemoryInstanceContext.
 
  The caller owns the object and should call `WasmEdge_MemoryInstanceDelete`
  to destroy it if the returned object is not added into a
  `WasmEdge_ModuleInstanceContext`.
 
  \param MemType the memory type context to initialize the memory instance
  context.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe MemoryInstanceCreate as ^ {`MemoryTypeContext'} -> `MemoryInstanceContext'#} 
{-|
  Get the memory type context from a memory instance.
 
  The memory type context links to the memory type in the memory instance
  context and owned by the context. The caller should __NOT__ call the
  `WasmEdge_MemoryTypeDelete`.
 
  \param Cxt the WasmEdge_MemoryInstanceContext.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe MemoryInstanceGetMemoryType as ^ {`MemoryInstanceContext'} -> `MemoryTypeContext'#} 
{#fun unsafe MemoryInstanceGetDataOut as memoryInstanceGetData_ {+,`MemoryInstanceContext',fromByteStringIn*`ByteString'&,`Word32'} -> `WasmResult'#}

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
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_MemoryInstanceContext.
  \param Data the data buffer to copy.
  \param Offset the data start offset in the memory instance.
  \param Length the data buffer length. If the `Offset + Length` is larger
  than the data size in the memory instance, this function will failed.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
W
  \param Cxt the WasmEdge_MemoryInstanceContext.
  \param Data the data buffer to copy.
  \param Offset the data start offset in the memory instance.
  \param Length the data buffer length. If the `Offset + Length` is larger
  than the data size in the memory instance, this function will failed.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe MemoryInstanceSetDataOut as memoryInstanceSetData {+,`MemoryInstanceContext',fromByteStringIn*`ByteString'&,`Word32'} -> `WasmResult'#}
{#fun unsafe MemoryInstanceGetPointer as memoryInstanceGetPointer_ {`MemoryInstanceContext',`Word32',`Word32'} -> `Ptr Word8'coercePtr#}

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
 
  \param Cxt the WasmEdge_MemoryInstanceContext.
 
  \returns the page size of the memory instance.
-}
{#fun unsafe MemoryInstanceGetPageSize as ^ {`MemoryInstanceContext'} -> `Word32'#} 
{-|
  Grow a memory instance with a page size.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_MemoryInstanceContext.
  \param Page the page count to grow in the memory instance.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe MemoryInstanceGrowPageOut as memoryInstanceGrowPage {+,`MemoryInstanceContext',`Word32'} -> `WasmResult'#} 

-- Global Instance
{-|
  Creation of the WasmEdge_GlobalInstanceContext.
 
  The caller owns the object and should call `WasmEdge_GlobalInstanceDelete`
  to destroy it if the returned object is not added into a
  `WasmEdge_ModuleInstanceContext`.
 
  \param GlobType the global type context to initialize the global instance
  context.
  \param Value the initial value with its value type of the global instance.
  This function will fail if the value type of `GlobType` and `Value` are not
  the same.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe GlobalInstanceCreateOut as globalInstanceCreate {`GlobalTypeContext',`WasmVal'} -> `GlobalInstanceContext'#}
{-|
  Get the global type context from a global instance.
 
  The global type context links to the global type in the global instance
  context and owned by the context. The caller should __NOT__ call the
  `WasmEdge_GlobalTypeDelete`.
 
  \param Cxt the WasmEdge_GlobalInstanceContext.
 
  \returns pointer to context, NULL if failed.
-}
{#fun unsafe GlobalInstanceGetGlobalType as ^ {`GlobalInstanceContext'} -> `GlobalTypeContext'#} 
{-|
  Get the value from a global instance.
 
  \param v the WasmVal reference in which the result would be stored
  \param Cxt the WasmEdge_GlobalInstanceContext.
 
  \returns the current value of the global instance.
-}
{#fun unsafe GlobalInstanceGetValueOut as globalInstanceGetValue  {+,`GlobalInstanceContext'} -> `WasmVal'#}
{-|
  Set the value from a global instance.
 
  This function will do nothing if the global context is set as the `Const`
  mutation or the value type not matched.
 
  \param Cxt the WasmEdge_GlobalInstanceContext.
  \param Value the value to set into the global context.
-}
{#fun unsafe GlobalInstanceSetValueOut as globalInstanceSetValue {`GlobalInstanceContext',`WasmVal'} -> `()'#} 

-- Calling Frame
{-|
  Get the executor context from the current calling frame.
 
  \param Cxt the WasmEdge_CallingFrameContext.
 
  \returns the executor context, NULL if the Cxt is NULL.
-}
{#fun unsafe CallingFrameGetExecutor as ^ {`CallingFrameContext'} -> `ExecutorContext'#}
{-|
  Get the module instance of the current calling frame.
 
  When a WASM function is executing and start to call a host function, a frame
  with the module instance which the WASM function belongs to will be pushed
  onto the stack. And therefore the calling frame context will record that
  module instance.
  So in one case that the module instance will be `NULL`: developers execute
  the function instance which is a host function and not added into a module
  instance.
 
  \param Cxt the WasmEdge_CallingFrameContext.
 
  \returns the module instance of the current calling frame.
-}
{#fun unsafe CallingFrameGetModuleInstance as ^ {`CallingFrameContext'} -> `ModuleInstanceContext'#}
{-|
  Get the memory instance by index from the module instance of the current
  calling frame.
 
  By default, a WASM module only have one memory instance after instantiation.
  Therefore, developers can use:
    `WasmEdge_CallingFrameGetMemoryInstance(Cxt, 0)`
  to get the memory instance in host function body.
  This extension is for the WASM multiple memories proposal. After enabling
  the proposal, there may be greater than 1 memory instances in a WASM module.
  So developers can use this function to access the memory instances which are
  not in 0 index.
 
  \param Cxt the WasmEdge_CallingFrameContext.
  \param Idx the index of memory instance in the module instance.
 
  \returns the memory instance, NULL if not found.
-}
{#fun unsafe CallingFrameGetMemoryInstance as ^ {`CallingFrameContext',`Word32'} -> `MemoryInstanceContext'#}

-- Async
{-|
  Wait a WasmEdge_Async execution.
 
  \param Cxt the WasmEdge_ASync.
-}
{#fun unsafe AsyncWait as ^ {`Async'} -> `()'#}
{-|
  Wait a WasmEdge_Async execution with timeout.
 
  \param Cxt the WasmEdge_ASync.
  \param Milliseconds times to wait.
 
  \returns Result of waiting, true for execution ended, false for timeout
  occurred.
-}
{#fun unsafe AsyncWaitFor as ^ {`Async',`Word64'} -> `Bool'#}

{- |
  Cancel a WasmEdge_Async execution.
 
  \param Cxt the WasmEdge_ASync.
-}
{#fun unsafe AsyncCancel as ^ {`Async'} -> `()'#}
{-|
  Wait and get the return list length of the WasmEdge_Async execution.
 
  This function will wait until the execution finished and return the return
  value list length of the executed function. This function will return 0 if
  the `Cxt` is NULL, the execution was failed, or the execution was canceled.
  Developers can call the `WasmEdge_AsyncGet` to get the execution status and
  the return values.
 
  \param Cxt the WasmEdge_ASync.
 
  \returns the return list length of the executed function.
-}
{#fun unsafe AsyncGetReturnsLength as ^ {`Async'} -> `Word32'#}
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
{#fun unsafe AsyncGetOut as asyncGet {+,`Async',`WasmVal',`Word32'} -> `WasmResult'#}

-- VM
{-|
  Register a module instance into the store in VM with exporting its module
  name.
 
  After calling this function, the existing module instance will be registered
  into the store context in this VM, and the other modules can import the
  exported instances for linking when instantiation. Developers SHOULD
  guarantee the life cycle of this existing module instance, or the error will
  occur when in execution after the module instance being destroyed if it has
  been imported by other modules. That is, developers should call the
  `WasmEdge_ModuleInstanceDelete` if this existing module instance will not be
  used anymore or after the deletion of this VM. When the module instance is
  deleted, it will be unregistered to the store context in this VM
  automatically.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext which contains the store.
  \param ImportCxt the WasmEdge_ModuleInstanceContext to register.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe VMRegisterModuleFromImportOut as vMRegisterModuleFromImport {+,`VMContext',`ModuleInstanceContext'} -> `WasmResult'#}
{-|
  Instantiate the WASM module from a WASM file and invoke a function by name.
 
  This is the function to invoke a WASM function rapidly.
  Load and instantiate the WASM module from the file path, and then invoke a
  function by name and parameters. If the `Returns` buffer length is smaller
  than the arity of the function, the overflowed return values will be
  discarded.
  After calling this function, a new module instance is instantiated, and the
  old one will be destroyed.
 
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

{#fun unsafe VMRunWasmFromFileOut as vMRunWasmFromFile {+,`VMContext',`String',%`WasmString',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `WasmResult'#}
{-|
  Instantiate the WASM module from a buffer and invoke a function by name.
 
  This is the function to invoke a WASM function rapidly.
  Load and instantiate the WASM module from a buffer, and then invoke a
  function by name and parameters. If the `Returns` buffer length is smaller
  than the arity of the function, the overflowed return values will be
  discarded.
  After calling this function, a new module instance is instantiated, and the
  old one will be destroyed.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
  \param Buf the buffer of WASM binary.
  \param BufLen the length of the buffer.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
  \param [out] Returns the WasmEdge_Value buffer to fill the return values.
  \param ReturnLen the return buffer length.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe VMRunWasmFromBufferOut as vMRunWasmFromBuffer {+,`VMContext',fromStoreVecOr0Ptr*`Vector Word8'& ,%`WasmString',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `WasmResult'#} 
{-|
  Instantiate the WASM module from a WasmEdge AST Module and invoke a function
  by name.
 
  This is the function to invoke a WASM function rapidly.
  Load and instantiate the WASM module from the WasmEdge AST Module, and then
  invoke the function by name and parameters. If the `Returns` buffer length
  is smaller than the arity of the function, the overflowed return values will
  be discarded.
  After calling this function, a new module instance is instantiated, and the
  old one will be destroyed.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
  \param ASTCxt the WasmEdge AST Module context generated by loader or
  compiler.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
  \param [out] Returns the WasmEdge_Value buffer to fill the return values.
  \param ReturnLen the return buffer length.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe VMRunWasmFromASTModuleOut as vMRunWasmFromASTModule {+,`VMContext',`ASTModuleContext',%`WasmString',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `WasmResult'#}

{-|
  Instantiate the WASM module from a WASM file and asynchronous invoke a
  function by name.
 
  This is the function to invoke a WASM function rapidly.
  Load and instantiate the WASM module from the file path, and then invoke a
  function by name and parameters. If the `Returns` buffer length is smaller
  than the arity of the function, the overflowed return values will be
  discarded.
  After calling this function, a new module instance is instantiated, and the
  old one will be destroyed.
 
  The caller owns the object and should call `WasmEdge_AsyncDelete` to destroy
  it.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
  \param Path the NULL-terminated C string of the WASM file path.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
 
  \returns WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call
  `WasmEdge_AsyncDelete` to destroy this object.
-}
{#fun unsafe VMAsyncRunWasmFromFileOut as vMAsyncRunWasmFromFile {`VMContext',`String',%`WasmString',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `Async'#}

{-|
  Instantiate the WASM module from a WasmEdge AST Module and asynchronous
  invoke a function by name.
 
  This is the function to invoke a WASM function rapidly.
  Load and instantiate the WASM module from the WasmEdge AST Module, and then
  invoke the function by name and parameters. If the `Returns` buffer length
  is smaller than the arity of the function, the overflowed return values will
  be discarded.
  After calling this function, a new module instance is instantiated, and the
  old one will be destroyed.
 
  The caller owns the object and should call `WasmEdge_AsyncDelete` to destroy
  it.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
  \param ASTCxt the WasmEdge AST Module context generated by loader or
  compiler.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
 
  \returns WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call
  `WasmEdge_AsyncDelete` to destroy this object.
-}
{#fun unsafe VMAsyncRunWasmFromASTModuleOut as vMAsyncRunWasmFromASTModule {`VMContext',`ASTModuleContext',%`WasmString',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `Async'#}
{-|
  Instantiate the WASM module from a buffer and asynchronous invoke a function
  by name.
 
  This is the function to invoke a WASM function rapidly.
  Load and instantiate the WASM module from a buffer, and then invoke a
  function by name and parameters. If the `Returns` buffer length is smaller
  than the arity of the function, the overflowed return values will be
  discarded.
  After calling this function, a new module instance is instantiated, and the
  old one will be destroyed.
 
  The caller owns the object and should call `WasmEdge_AsyncDelete` to destroy
  it.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
  \param Buf the buffer of WASM binary.
  \param BufLen the length of the buffer.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
 
  \returns WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call
  `WasmEdge_AsyncDelete` to destroy this object.
-}
{#fun unsafe VMAsyncRunWasmFromBufferOut as vMAsyncRunWasmFromBuffer {`VMContext',fromStoreVecOr0Ptr*`Vector Word8'&, %`WasmString',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `Async'#}
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
{#fun unsafe VMCreate as ^ {`ConfigureContext',`StoreContext'} -> `VMContext'#}
{-|
  Register and instantiate WASM into the store in VM from a WASM file.
 
  Load a WASM file from the path, and register all exported instances and
  instantiate them into the store into the VM with their exported name and
  module name.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext which contains the store.
  \param ModuleName the WasmEdge_String of module name for all exported
  instances.
  \param Path the NULL-terminated C string of the WASM file path.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe VMRegisterModuleFromFileOut as vMRegisterModuleFromFile {+,`VMContext',%`WasmString',`String'} -> `WasmResult'#}
{-|
  Register and instantiate WASM into the store in VM from a buffer.
 
  Load a WASM module from a buffer, and register all exported instances and
  instantiate them into the store into the VM with their exported name and
  module name.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext which contains the store.
  \param ModuleName the WasmEdge_String of module name for all exported
  instances.
  \param Buf the buffer of WASM binary.
  \param BufLen the length of the buffer.
-}
{#fun unsafe VMRegisterModuleFromBufferOut as vMRegisterModuleFromBuffer {+,`VMContext',%`WasmString',fromStoreVecOr0Ptr*`Vector Word8'& } -> `WasmResult'#}
{-|
  Instantiate and register an AST Module into a named module instance in VM.
 
  Load from the AST Module, and register all exported instances and
  instantiate them into the store in VM with their exported name and module
  name.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext which contains the store.
  \param ModuleName the WasmEdge_String of module name for all exported
  instances.
  \param ASTCxt the WasmEdge AST Module context generated by loader or
  compiler.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe VMRegisterModuleFromASTModuleOut as vMRegisterModuleFromASTModule {+,`VMContext',%`WasmString',`ASTModuleContext'} -> `WasmResult'#}
{-|
  Load the WASM module from a WASM file.
 
  This is the first step to invoke a WASM function step by step.
  Load and parse the WASM module from the file path. You can then call
  `WasmEdge_VMValidate` for the next step.
 
  This function is thread-safe.
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
  \param Path the NULL-terminated C string of the WASM file path.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe VMLoadWasmFromFileOut as vMLoadWasmFromFile {+,`VMContext',`String'} -> `WasmResult'#}
{-|
  Load the WASM module from a buffer.
 
  This is the first step to invoke a WASM function step by step.
  Load and parse the WASM module from a buffer. You can then call
  `WasmEdge_VMValidate` for the next step.
 
  This function is thread-safe.

  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
  \param Buf the buffer of WASM binary.
  \param BufLen the length of the buffer.
-}
{#fun unsafe VMLoadWasmFromBufferOut as vMLoadWasmFromBuffer {+,`VMContext',fromStoreVecOr0Ptr*`Vector Word8'& } -> `WasmResult'#}

{-|
  Load the WASM module from loaded WasmEdge AST Module.
 
  This is the first step to invoke a WASM function step by step.
  Copy the loaded WasmEdge AST Module context into VM. The VM context has no
  dependency on the input AST Module context. You can then call
  `WasmEdge_VMValidate` for the next step.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
  \param ASTCxt the WasmEdge AST Module context generated by loader or
  compiler.
-}
{#fun unsafe VMLoadWasmFromASTModuleOut as vMLoadWasmFromASTModule {+,`VMContext',`ASTModuleContext'} -> `WasmResult'#}

{-|
  Validate the WASM module loaded into the VM context.
 
  This is the second step to invoke a WASM function step by step.
  After loading a WASM module into VM context, You can call this function to
  validate it. And you can then call `WasmEdge_VMInstantiate` for the next
  step. Note that only validated WASM modules can be instantiated in the VM
  context.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
-}
{#fun unsafe VMValidateOut as vMValidate  {+,`VMContext'} -> `WasmResult'#}

{-|
  Instantiate the validated WASM module in the VM context.
 
  This is the third step to invoke a WASM function step by step.
  After validating a WASM module in the VM context, You can call this function
  to instantiate it. And you can then call `WasmEdge_VMExecute` for invoking
  the exported function in this WASM module.
  After calling this function, a new module instance is instantiated, and the
  old one will be destroyed.
 
  This function is thread-safe.

  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
 
  \returns WasmEdge_Result. Call `WasmEdge_ResultGetMessage` for the error
  message.
-}
{#fun unsafe VMInstantiateOut as vMInstantiate  {+,`VMContext'} -> `WasmResult'#}

{-|
  Invoke a WASM function by name.
 
  This is the final step to invoke a WASM function step by step.
  After instantiating a WASM module in the VM context, the WASM module is
  registered into the store in the VM context as an anonymous module. Then you
  can repeatedly call this function to invoke the exported WASM functions by
  their names until the VM context is reset or a new WASM module is registered
  or loaded. For calling the functions in registered WASM modules with module
  names, please use `WasmEdge_VMExecuteRegistered` instead. If the `Returns`
  buffer length is smaller than the arity of the function, the overflowed
  return values will be discarded.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
  \param [out] Returns the WasmEdge_Value buffer to fill the return values.
  \param ReturnLen the return buffer length.
 
  message.
-}
{#fun unsafe VMExecuteOut as vMExecute {+,`VMContext',%`WasmString', fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `WasmResult'#}
{-|
  Invoke a WASM function by its module name and function name.
 
  After registering a WASM module in the VM context, you can repeatedly call
  this function to invoke exported WASM functions by their module names and
  function names until the VM context is reset. If the `Returns` buffer length
  is smaller than the arity of the function, the overflowed return values will
  be discarded.
 
  This function is thread-safe.
 
  \param resOut reference of wasmResult in which the result would be stored
  \param Cxt the WasmEdge_VMContext.
  \param ModuleName the module name WasmEdge_String.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
  \param [out] Returns the WasmEdge_Value buffer to fill the return values.
  \param ReturnLen the return buffer length.
-}
{#fun unsafe VMExecuteRegisteredOut as vMExecuteRegistered {+,`VMContext',%`WasmString',%`WasmString', fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&,fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `WasmResult'#}
{-|
  Asynchronous invoke a WASM function by name.
 
  This is the final step to invoke a WASM function step by step.
  After instantiating a WASM module in the VM context, the WASM module is
  registered into the store in the VM context as an anonymous module. Then you
  can repeatedly call this function to invoke the exported WASM functions by
  their names until the VM context is reset or a new WASM module is registered
  or loaded. For calling the functions in registered WASM modules with module
  names, please use `WasmEdge_VMAsyncExecuteRegistered` instead.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
 
  \returns WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call
  `WasmEdge_AsyncDelete` to destroy this object.
-}
{#fun unsafe VMAsyncExecuteOut as vMAsyncExecute {`VMContext',%`WasmString',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `Async'#}
{-|
  Asynchronous invoke a WASM function by its module name and function name.
 
  After registering a WASM module in the VM context, you can repeatedly call
  this function to invoke exported WASM functions by their module names and
  function names until the VM context is reset.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
  \param ModuleName the module name WasmEdge_String.
  \param FuncName the function name WasmEdge_String.
  \param Params the WasmEdge_Value buffer with the parameter values.
  \param ParamLen the parameter buffer length.
 
  \returns WasmEdge_Async. Call `WasmEdge_AsyncGet` for the result, and call
  `WasmEdge_AsyncDelete` to destroy this object.
-}
{#fun unsafe VMAsyncExecuteRegisteredOut as vMAsyncExecuteRegistered {`VMContext',%`WasmString',%`WasmString',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmVal)'&} -> `Async'#} 
{-|
  Get the function type by function name.
 
  After instantiating a WASM module in the VM context, the WASM module is
  registered into the store in the VM context as an anonymous module. Then you
  can call this function to get the function type by the exported function
  name until the VM context is reset or a new WASM module is registered or
  loaded. For getting the function type of functions in registered WASM
  modules with module names, please use `WasmEdge_VMGetFunctionTypeRegistered`
  instead.
  The returned function type context are linked to the context owned by the VM
  context, and the caller should __NOT__ call the
  `WasmEdge_FunctionTypeDelete` to destroy it.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
  \param FuncName the function name WasmEdge_String.
 
  \returns the function type. NULL if the function not found.
-}
{#fun unsafe VMGetFunctionType as ^ {`VMContext',%`WasmString'} -> `FunctionTypeContext'#}
{-|
  Get the function type by function name.
 
  After registering a WASM module in the VM context, you can call this
  function to get the function type by the functions' exported module names
  and function names until the VM context is reset.
  The returned function type context are linked to the context owned by the VM
  context, and the caller should __NOT__ call the
  `WasmEdge_FunctionTypeDelete` to destroy it.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
  \param ModuleName the module name WasmEdge_String.
  \param FuncName the function name WasmEdge_String.
 
  \returns the function type. NULL if the function not found.
-}
{#fun unsafe VMGetFunctionTypeRegistered as ^ {`VMContext',%`WasmString',%`WasmString'} -> `FunctionTypeContext'#}
{-|
  Reset of WasmEdge_VMContext.
 
  After calling this function, the statistics, loaded module, the instantiated
  instances, and the registered instances except the WASI and plug-ins will
  all be cleared.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext to reset.
-}
{#fun unsafe VMCleanup as ^ {`VMContext'} -> `()'#} 

{-|
  Get the length of exported function list.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
 
  \returns length of exported function list.
-}
{#fun unsafe VMGetFunctionListLength as ^ {`VMContext'} -> `Word32'#} 
{#fun unsafe VMGetFunctionListOut as vmGetFunctionList_ {`VMContext', fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&, fromMutIOVecOr0Ptr*`IOVector (Ptr FunctionTypeContext)'&} -> `Word32'#}

vMGetFunctionList ::
  VMContext
  -> Word32
  -> IO (V.Vector WasmString, V.Vector FunctionTypeContext)
vMGetFunctionList vmcxt sz = do
  namesVSM <- VSM.new (fromIntegral sz)
  ftypesVSM <- VSM.new (fromIntegral sz)
  listSz <- vmGetFunctionList_ vmcxt namesVSM ftypesVSM
  names <- V.generateM (fromIntegral listSz) ((noFinalizer =<<) . (VSM.read namesVSM))
  ftypes <- V.generateM (fromIntegral listSz) ((noFinalizer =<<) . (VSM.read ftypesVSM))
  pure (names, ftypes)
  
{-|
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
  \param Reg the host registration value to get the import module.
 
  \returns pointer to the module instance context. NULL if not found.
-}
{#fun unsafe VMGetImportModuleContext as ^ {`VMContext',`HostRegistration'} -> `ModuleInstanceContext'#} 
{-|
  Get the current instantiated module in VM.
 
  After instantiating a module instance into the VM, developers can call this
  API to get the module instance to retrieve the exported instances. The
  module instance context links to the context owned by the VM context. The
  caller should __NOT__ call the `WasmEdge_ModuleInstanceDelete`.
 
  This function is thread-safe.
 
  \param Cxt the WasmEdge_VMContext.
 
  \returns pointer to the module instance context. NULL if not found.
-}
{#fun unsafe VMGetActiveModule as ^ {`VMContext'} -> `ModuleInstanceContext'#} 
{-|
  Get the registered module in VM by the module name.
  After registering a WASM module into the VM context, developers can call
  this function to get the module instance by the module name. The returned
  module instance context links to the context owned by the VM context, and
  the caller should __NOT__ call the `WasmEdge_ModuleInstanceDelete` to
  destroy it.
  This function is thread-safe.
  \param Cxt the WasmEdge_VMContext.
  \param ModuleName the module name WasmEdge_String.
  \returns pointer to the module instance context. NULL if not found.
-}
{#fun unsafe VMGetRegisteredModule as ^ {`VMContext',%`WasmString'} -> `ModuleInstanceContext'#}
{-|
  Get the length of registered module list in the WasmEdge_VMContext.
  This function is thread-safe.
  \param Cxt the WasmEdge_VMContext.
  \returns length of registered module list.
-}
{#fun unsafe VMListRegisteredModuleLength as ^ {`VMContext'} -> `Word32'#} 
{-|
  List the registered module names in the WasmEdge_VMContext.
  This function will list all registered module names.
  The returned module names filled into the `Names` array are linked to the
  registered module names in the VM context, and the caller should __NOT__
  call the `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the registered
  named module list size, the overflowed return values will be discarded.
  This function is thread-safe.
  \param Cxt the WasmEdge_VMContext.
  \param [out] Names the output names WasmEdge_String buffer of the registered
  modules.
  \param Len the buffer length.
  \returns actual registered module list size.
-}
{#fun unsafe VMListRegisteredModuleOut as vMListRegisteredModule {`VMContext',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&} -> `Word32'#} 
{-|
  Get the store context used in the WasmEdge_VMContext.
  The returned store context links to the store in the VM context and owned by
  the VM context. This function will return NULL if error occurs. The caller
  should __NOT__ call the `WasmEdge_StoreDelete`.
  This function is thread-safe.
  \param Cxt the WasmEdge_VMContext.
  \returns pointer to the store context.
-}
{#fun unsafe VMGetStoreContext as ^ {`VMContext'} -> `StoreContext'#} 
{-|
  Get the loader context used in the WasmEdge_VMContext.
  The returned loader context links to the loader in the VM context and owned
  by the VM context. This function will return NULL if error occurs. The
  caller should __NOT__ call the `WasmEdge_LoaderDelete`.
  This function is thread-safe.
  \param Cxt the WasmEdge_VMContext.
  \returns pointer to the loader context.
-}
{#fun unsafe VMGetLoaderContext as ^ {`VMContext'} -> `LoaderContext'#} 
{-|
  Get the validator context used in the WasmEdge_VMContext.
  The returned validator context links to the validator in the VM context and
  owned by the VM context. This function will return NULL if error occurs. The
  caller should __NOT__ call the `WasmEdge_ValidatorDelete`.
  This function is thread-safe.
  \param Cxt the WasmEdge_VMContext.
  \returns pointer to the validator context.
-}
{#fun unsafe VMGetValidatorContext as ^ {`VMContext'} -> `ValidatorContext'#} 
{-|
  Get the executor context used in the WasmEdge_VMContext.
  The returned executor context links to the executor in the VM context and
  owned by the VM context. This function will return NULL if error occurs. The
  caller should __NOT__ call the `WasmEdge_ExecutorDelete`.
  This function is thread-safe.
  \param Cxt the WasmEdge_VMContext.
  \returns pointer to the executor context.
-}
{#fun unsafe VMGetExecutorContext as ^ {`VMContext'} -> `ExecutorContext'#} 
{-|
  Get the statistics context used in the WasmEdge_VMContext.
  The statistics context links to the statistics in the VM context and owned
  by the VM context. The caller should __NOT__ call the
  `WasmEdge_StatisticsDelete`.
  This function is thread-safe.
  \param Cxt the WasmEdge_VMContext.
  \returns pointer to the statistics context.
-}
{#fun unsafe VMGetStatisticsContext as ^ {`VMContext'} -> `StatisticsContext'#} 

-- Driver
{-|
  Entrypoint for the compiler tool.
  This function provides an entrypoint to the WasmEdge AOT compiler tool with
  the command line arguments.
  \param Argc the argument count.
  \param Argv the argument vector.
  \returns the execution status.
-}
{#fun unsafe Driver_Compiler as ^ {fromVecStringOr0Ptr*`V.Vector String'&} -> `Int32'#}
{-|
  Entrypoint for the runtime tool.
  This function provides an entrypoint to the WasmEdge runtime tool with the
  command line arguments.
  \param Argc the argument count.
  \param Argv the argument vector.
  \returns the execution status.
-}
{#fun unsafe Driver_Tool as ^ {fromVecStringOr0Ptr*`V.Vector String'&} -> `Int32'#}
{-|
  Entrypoint for the unified tool.
  This function provides an entrypoint to the WasmEdge unified tool with the
  command line arguments.
  \param Argc the argument count.
  \param Argv the argument vector.
  \returns the execution status.
-}
{#fun unsafe Driver_UniTool as ^ {fromVecStringOr0Ptr*`V.Vector String'&} -> `Int32'#}

-- Plugin Function
{-|
  Load plugins with the default search paths.
 
  The default paths are:
    1. The environment variable "WASMEDGE_PLUGIN_PATH".
    2. The "../plugin/" directory related to the WasmEdge installation path.
    3. The "wasmedge/" directory under the library path if the WasmEdge is
       installed under the "/usr".
-}
{#fun unsafe PluginLoadWithDefaultPaths as ^ {} -> `()'#} 
{-|
  Load the plugin with the given file or directory.
 
  For the given file path, this function will load the plug-in.
  For the given directory path, this function will load the plug-ins under the
  directory recursively.
 
  \param Path the path to plug-in file or directory.
-}
{#fun unsafe PluginLoadFromPath as ^ {`String'} -> `()'#}
{-|
  Get the length of loaded plug-in list.
 
  \returns length of loaded plug-in list.
-}
{#fun unsafe PluginListPluginsLength as ^ {} -> `Word32'#} 
{-|
  List the loaded plug-ins with their names.
 
  The returned plug-in names filled into the `Names` array are owned by the
  internal WasmEdge plug-in storage, and the caller should __NOT__ call the
  `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the loaded
  plug-in list size, the overflowed return values will be discarded.
 
  \param [out] Names the output WasmEdge_String buffer of the function names.
  \param Len the buffer length.
 
  \returns actual loaded plug-in list size.
-}
{#fun unsafe PluginListPluginsOut as pluginListPlugins {fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&} -> `Word32'#} 
{-|
  Find the loaded plug-in context by name.
 
  After loading the plug-ins from default paths or the given path, developers
  can use this API to retrieve the plug-in context by name. Then developers
  can create the module instance from the plug-in contexts.
 
  \param Name the plug-in name WasmEdge_String.
 
  \returns pointer to the plug-in context. NULL if the plug-in not found.
-}
{#fun unsafe PluginFind as ^ {%`WasmString'} -> `PluginContext'#} 
{-|
  Get the plug-in name of the plug-in context.
 
  The returned string object is linked to the plug-in name of the plug-in
  context, and the caller should __NOT__ call the `WasmEdge_StringDelete`.
  \param strOut WasmEdge_String* in which the result would be stored
  \param Cxt the WasmEdge_PluginContext.
 
-}
{#fun unsafe PluginGetPluginNameOut as pluginGetPluginName {+,`PluginContext'} -> `WasmString'#} 
{-|
  Get the length of module list in the plug-in context.
 
  There may be several modules in a plug-in. Developers can use this function
  to get the length of the module list in a plug-in.
 
  \param Cxt the WasmEdge_PluginContext to get the length of the module list.
 
  \returns length of module list.
-}
{#fun unsafe PluginListModuleLength as ^ {`PluginContext'} -> `Word32'#} 
{-|
  List the modules in the plug-in context with their names.
 
  The returned module names filled into the `Names` array are owned by the
  internal WasmEdge plug-in storage, and the caller should __NOT__ call the
  `WasmEdge_StringDelete`.
  If the `Names` buffer length is smaller than the result of the loaded
  plug-in list size, the overflowed return values will be discarded.
 
  \param Cxt the WasmEdge_PluginContext to list the modules.
  \param [out] Names the output WasmEdge_String buffer of the function names.
  \param Len the buffer length.
 
  \returns actual module list size of the plug-in.
-}
{#fun unsafe PluginListModuleOut as pluginListModule {`PluginContext',fromMutIOVecOr0Ptr*`IOVector (Ptr WasmString)'&} -> `Word32'#} 
{-|
- Create the module instance in the plug-in by the module name.
- By giving the module name, developers can retrieve the module in the plug-in
  and create the module instance.
  The caller owns the object and should call `WasmEdge_ModuleInstanceDelete`
  to destroy it.
 
  \param Cxt the WasmEdge_PluginContext to retrieve and create module.
  \param ModuleName the module name to retrieve.
 
  \returns pointer to the module instance context, NULL if the module name not
  found in the plug-in or the plug-in is not valid.
-}
{#fun unsafe PluginCreateModule as ^ {`PluginContext',%`WasmString'} -> `ModuleInstanceContext'#} 
{-|
  Implement by plugins for returning the plugin descriptor.
  \returns the plugin descriptor.
-}
-- TODO:
-- {#fun unsafe Plugin_GetDescriptor as ^ {} -> `PluginDescriptor'#} 
