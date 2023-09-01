{-|
Module      : WasmEdge.Internal.FFI.ValueTypes
Description : Haskell bindings for wasmedge runtime hosting
Copyright   : (c) ByteAlly, 2023
License     : GPL-3
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
  ,WasmVal
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
  ,WasmHostFunc 
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
  ,functionInstanceGetFunctionType 
  ,tableInstanceCreate
  ,tableInstanceGetTableType
  ,tableInstanceGetData
  ,tableInstanceSetData
  ,tableInstanceGetSize
  ,tableInstanceGrow
  ,memoryInstanceCreate
  ,memoryInstanceGetMemoryType
  ,memoryInstanceGetData
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
  ,pluginGetDescriptor
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
  -- something
  ,fromHsRefIn
  ,fromHsRef
  ,toHsRef
  ,fromI128
  ,fromI128Alloc
  ,toI128
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
  ,fromVecOr0Ptr
  ,fromVecStringOr0Ptr
  ,fromMutIOVecOr0Ptr
  ,noFinalizer 
  ,peekOutPtr
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
-- import GHC.Ptr
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Bifunctor
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
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

void ExecutorRegisterOut(WasmEdge_Result* resOut, WasmEdge_ExecutorContext *Cxt, WasmEdge_ModuleInstanceContext **ModuleCxt, WasmEdge_StoreContext *StoreCxt, const WasmEdge_ASTModuleContext *ASTCxt,WasmEdge_String* ModuleName)
{
  *resOut = WasmEdge_ExecutorRegister(Cxt, ModuleCxt, StoreCxt, ASTCxt, *ModuleName);
}

void TableTypeGetLimitOut(WasmEdge_Limit* limOut,const WasmEdge_TableTypeContext *Cxt){ *limOut = WasmEdge_TableTypeGetLimit(Cxt); }
void MemoryTypeGetLimitOut(WasmEdge_Limit* limOut,const WasmEdge_MemoryTypeContext *Cxt)
{
  *limOut = WasmEdge_MemoryTypeGetLimit(Cxt); 
}
void ExecutorRegisterImportOut(WasmEdge_Result* resOut, WasmEdge_ExecutorContext *Cxt, WasmEdge_StoreContext *StoreCxt,const WasmEdge_ModuleInstanceContext *ImportCxt){ 
*resOut = WasmEdge_ExecutorRegisterImport(Cxt,StoreCxt,ImportCxt); }
void ExecutorInvokeOut(WasmEdge_Result *resOut, WasmEdge_ExecutorContext *Cxt,const WasmEdge_FunctionInstanceContext *FuncCxt,
  const WasmVal *v1, const uint32_t ParamLen,WasmVal *v2, const uint32_t ReturnLen)
{
    WasmEdge_Value Params = {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
    WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
    *resOut = WasmEdge_ExecutorInvoke (Cxt,FuncCxt,&Params,ParamLen,&Returns,ReturnLen); //Is this correct??
}
WasmEdge_Async *ExecutorAsyncInvokeOut(WasmEdge_ExecutorContext *Cxt,const WasmEdge_FunctionInstanceContext *FuncCxt,const WasmVal *v,const uint32_t ParamLen)
{
    WasmEdge_Value Params = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
    return WasmEdge_ExecutorAsyncInvoke(Cxt,FuncCxt,&Params,ParamLen);
}
void ModuleInstanceGetModuleNameOut(WasmEdge_String* strOut,WasmEdge_ModuleInstanceContext* Ctx){ *strOut = WasmEdge_ModuleInstanceGetModuleName(Ctx); }
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
void MemoryInstanceGetDataOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext* Ctx,uint8_t *Data, const uint32_t Offset,const uint32_t Length){ 
*resOut = WasmEdge_MemoryInstanceGetData(Ctx,Data,Offset,Length); }
void MemoryInstanceSetDataOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext* Ctx,uint8_t *Data, const uint32_t Offset,const uint32_t Length){ 
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
void VMRunWasmFromFileOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const char *Path, const WasmEdge_String FuncName,const WasmVal *v1, const uint32_t ParamLen,WasmVal *v2, const uint32_t ReturnLen)
{
  WasmEdge_Value Params= {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMRunWasmFromFile(Cxt,Path,FuncName,&Params,ParamLen,&Returns,ReturnLen);
}
void VMRunWasmFromASTModuleOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_ASTModuleContext *ASTCxt,const WasmEdge_String FuncName, const WasmVal *v1,const uint32_t ParamLen, WasmVal *v2, const uint32_t ReturnLen)
{
  WasmEdge_Value Params= {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMRunWasmFromASTModule(Cxt,ASTCxt,FuncName,&Params,ParamLen,&Returns,ReturnLen); 
}
WasmEdge_Async *VMAsyncRunWasmFromFileOut(WasmEdge_VMContext *Cxt, const char *Path, const WasmEdge_String FuncName,const WasmVal *v, const uint32_t ParamLen)
{
  WasmEdge_Value Params= {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_VMAsyncRunWasmFromFile(Cxt,Path,FuncName,&Params,ParamLen);
}
WasmEdge_Async *VMAsyncRunWasmFromASTModuleOut(WasmEdge_VMContext *Cxt,const WasmEdge_ASTModuleContext *ASTCxt,const WasmEdge_String FuncName,const WasmVal *v,const uint32_t ParamLen)
{
  WasmEdge_Value Params= {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_VMAsyncRunWasmFromASTModule(Cxt,ASTCxt,FuncName,&Params,ParamLen);
}
void VMExecuteOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_String FuncName,const WasmVal *v1, const uint32_t ParamLen,WasmVal *v2, const uint32_t ReturnLen)
{
  WasmEdge_Value Params = {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMExecute(Cxt,FuncName,&Params,ParamLen,&Returns,ReturnLen);
}
void VMExecuteRegisteredOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_String ModuleName,const WasmEdge_String FuncName, const WasmVal *v1,const uint32_t ParamLen, WasmVal *v2, const uint32_t ReturnLen)
{
  WasmEdge_Value Params = {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMExecuteRegistered(Cxt,ModuleName,FuncName,&Params,ParamLen,&Returns,ReturnLen); 
}
WasmEdge_Async *VMAsyncExecuteOut(WasmEdge_VMContext *Cxt, const WasmEdge_String FuncName,const WasmVal *v, const uint32_t ParamLen)
{
   WasmEdge_Value Params = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
   return WasmEdge_VMAsyncExecute(Cxt,FuncName,&Params,ParamLen);
}
WasmEdge_Async* VMAsyncExecuteRegisteredOut(WasmEdge_VMContext *Cxt, const WasmEdge_String ModuleName,const WasmEdge_String FuncName, const WasmVal *v,const uint32_t ParamLen)
{
   WasmEdge_Value Params = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
   return WasmEdge_VMAsyncExecuteRegistered(Cxt,ModuleName,FuncName,&Params,ParamLen);
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
WasmEdge_Async* VMAsyncRunWasmFromBufferOut(WasmEdge_VMContext *Cxt, const uint8_t *Buf, const uint32_t BufLen,const WasmEdge_String FuncName,WasmVal* v,const uint32_t ParamLen )
{
  WasmEdge_Value Params = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_VMAsyncRunWasmFromBuffer(Cxt,Buf,BufLen,FuncName,&Params,ParamLen);
}
void VMLoadWasmFromBufferOut(WasmEdge_Result* resOut,WasmEdge_VMContext *Cxt,const uint8_t *Buf, const uint32_t BufLen){ *resOut = WasmEdge_VMLoadWasmFromBuffer(Cxt,Buf,BufLen); }
void VMRegisterModuleFromBufferOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt,const WasmEdge_String ModuleName,const uint8_t *Buf, const uint32_t BufLen )
{
  *resOut = WasmEdge_VMRegisterModuleFromBuffer(Cxt,ModuleName,Buf,BufLen); 
}
void VMRunWasmFromBufferOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const uint8_t *Buf, const uint32_t BufLen,const WasmEdge_String FuncName, const WasmVal *v1,
  const uint32_t ParamLen, WasmVal *v2, const uint32_t ReturnLen)
{
  WasmEdge_Value Params = {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMRunWasmFromBuffer(Cxt,Buf,BufLen,FuncName,&Params,ParamLen,&Returns,ReturnLen);
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
-- Program option for plugins.
{#pointer *WasmEdge_ProgramOption as ProgramOption foreign newtype #}
-- Module descriptor for plugins.
{#pointer *WasmEdge_ModuleDescriptor as ModuleDescriptor foreign newtype #}
-- Version data for plugins.
{#pointer *WasmEdge_PluginVersionData as PluginVersionData foreign newtype #}
-- Plugin descriptor for plugins.
{#pointer *WasmEdge_PluginDescriptor as PluginDescriptor foreign newtype #}

---
fromHsRefIn :: HsRef -> (Ptr HsRefPtr -> IO a) -> IO a
fromHsRefIn (HsRef (Fingerprint hi lo) sp) f = do
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
{#fun pure unsafe ValueGetI32 as ^ {`WasmVal'} -> `Int32' #}

{#fun pure unsafe ValueGenI64 as ^ {+, `Int64'} -> `WasmVal' #}
{#fun pure unsafe ValueGetI64 as ^ {`WasmVal'} -> `Int64' #}

{#fun pure unsafe ValueGenF32 as ^ {+, `Float'} -> `WasmVal' #}
{#fun pure unsafe ValueGetF32 as ^ {`WasmVal'} -> `Float' #}

{#fun pure unsafe ValueGenF64 as ^ {+, `Double'} -> `WasmVal' #}
{#fun pure unsafe ValueGetF64 as ^ {`WasmVal'} -> `Double' #}

{#fun pure unsafe ValueGenV128 as ^ {+, fromI128*`Int128'} -> `WasmVal' #}
{#fun pure unsafe ValueGetV128 as ^ {`WasmVal', fromI128Alloc-`Int128'toI128*} -> `()' #}

{#fun pure unsafe ValueGenNullRef as ^ {+, cFromEnum`RefType'} -> `WasmVal' #}
{#fun pure unsafe ValueIsNullRef as ^ {`WasmVal'} -> `Bool' #}

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

{#fun pure unsafe ResultGenOut as resultGen {+, cFromEnum`ErrCategory', `CUInt'} -> `WasmResult' #}
{#fun pure unsafe WasmEdge_ResultGetCode as getResultCode {%`WasmResult'} -> `Word32' #}
{#fun pure unsafe WasmEdge_ResultGetCategory as getResultCategory {%`WasmResult'} -> `ErrCategory'cToEnum #}
{#fun pure unsafe WasmEdge_ResultGetMessage as getResultMessage {%`WasmResult'} -> `ByteString'packCStringBS* #}
{#fun pure unsafe WasmEdge_LimitIsEqual as limitEq_ {%`Limit',%`Limit'} -> `Bool'#}

instance Eq Limit where
  (==) = limitEq_

{-|
  Opaque struct of WasmEdge configure.
-}
{#pointer *ConfigureContext as ^ foreign finalizer ConfigureDelete as ^ newtype #}
{#pointer *StatisticsContext as ^ foreign finalizer StatisticsDelete as ^ newtype #}
{#pointer *ASTModuleContext as ^ foreign finalizer ASTModuleDelete as ^ newtype #}

{-|
Opaque struct of WasmEdge function type.
-}
{#pointer *FunctionTypeContext as ^ foreign finalizer FunctionTypeDelete as ^ newtype #}
{#pointer *MemoryTypeContext as ^ foreign finalizer MemoryTypeDelete as ^ newtype #}
{#pointer *TableTypeContext as ^ foreign finalizer TableTypeDelete as ^ newtype #}
{#pointer *GlobalTypeContext as ^ foreign finalizer GlobalTypeDelete as ^ newtype #}
{#pointer *ImportTypeContext as ^ newtype #}
{#pointer *ExportTypeContext as ^ newtype #}
{#pointer *CompilerContext as ^ foreign finalizer CompilerDelete as ^ newtype #}
{#pointer *LoaderContext as ^ foreign finalizer LoaderDelete as ^ newtype #}
{#pointer *ValidatorContext as ^ foreign finalizer ValidatorDelete as ^ newtype #}
{#pointer *ExecutorContext as ^ foreign finalizer ExecutorDelete as ^ newtype #}
{#pointer *StoreContext as ^ foreign finalizer StoreDelete as ^ newtype #}
{#pointer *ModuleInstanceContext as ^ foreign finalizer ModuleInstanceDelete as ^ newtype #}
{#pointer *FunctionInstanceContext as ^ foreign finalizer FunctionInstanceDelete as ^ newtype #}
{#pointer *TableInstanceContext as ^ foreign finalizer TableInstanceDelete as ^ newtype #}
{#pointer *MemoryInstanceContext as ^ foreign finalizer MemoryInstanceDelete as ^ newtype #}
{#pointer *GlobalInstanceContext as ^ foreign finalizer GlobalInstanceDelete as ^ newtype #}
{#pointer *CallingFrameContext as ^ foreign newtype #}
{#pointer *Async as ^ foreign finalizer AsyncDelete as ^ newtype #}
{#pointer *VMContext as ^ foreign finalizer VMDelete as ^ newtype #}
{#pointer *PluginContext as ^ foreign newtype #}

{#fun pure unsafe ValueGenFuncRef as ^ {+, `FunctionInstanceContext'} -> `WasmVal' #}
{#fun pure unsafe ValueGetFuncRef as ^ {`WasmVal'} -> `FunctionInstanceContext' #}

{#fun pure unsafe ValueGenExternRef as ^ {+, fromHsRefIn*`HsRef'} -> `WasmVal' #}
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
  deriving (Show, Eq)
#}
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
{#fun unsafe ConfigureCreate as ^ {} -> `ConfigureContext'#}
{#fun unsafe ConfigureAddProposal as ^ {`ConfigureContext',`Proposal'} -> `()'#}
{#fun unsafe ConfigureRemoveProposal as ^ {`ConfigureContext',`Proposal'} -> `()'#}
{#fun unsafe ConfigureHasProposal as ^ {`ConfigureContext',`Proposal'} -> `Bool'#}

{-|
  Add a built-in host registration setting into WasmEdge_ConfigureContext.
-}
{#fun unsafe ConfigureAddHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `()'#}
{#fun unsafe ConfigureRemoveHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `()'#}
{#fun unsafe ConfigureHasHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `Bool'#}
{#fun unsafe ConfigureSetMaxMemoryPage as ^ {`ConfigureContext', `Word32'} -> `()'#}
{#fun unsafe ConfigureGetMaxMemoryPage as ^ {`ConfigureContext'} -> `Word32'#}
{#fun unsafe ConfigureSetForceInterpreter as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureIsForceInterpreter as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureCompilerSetOptimizationLevel as ^ {`ConfigureContext', `CompilerOptimizationLevel'} -> `()'#}
{#fun unsafe ConfigureCompilerGetOptimizationLevel as ^ {`ConfigureContext'} -> `CompilerOptimizationLevel'#}
{#fun unsafe ConfigureCompilerSetOutputFormat as ^ {`ConfigureContext', `CompilerOutputFormat'} -> `()'#}
{#fun unsafe ConfigureCompilerGetOutputFormat as ^ {`ConfigureContext'} -> `CompilerOutputFormat'#}
{#fun unsafe ConfigureCompilerSetDumpIR as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureCompilerIsDumpIR as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureCompilerSetGenericBinary as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureCompilerIsGenericBinary as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureCompilerSetInterruptible as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureCompilerIsInterruptible as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureStatisticsSetInstructionCounting as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureStatisticsIsInstructionCounting as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureStatisticsSetCostMeasuring as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureStatisticsIsCostMeasuring as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureStatisticsSetTimeMeasuring as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureStatisticsIsTimeMeasuring as ^ {`ConfigureContext'} -> `Bool'#}

-- Statistics
{#fun unsafe StatisticsCreate as ^ {} -> `StatisticsContext'#}
{#fun unsafe StatisticsGetInstrCount as ^ {`StatisticsContext'} -> `Word64'#}
{#fun unsafe StatisticsGetInstrPerSecond as ^ {`StatisticsContext'} -> `Double'#}
{#fun unsafe StatisticsGetTotalCost as ^ {`StatisticsContext'} -> `Word64'#}
{#fun unsafe StatisticsSetCostTable as ^ {`StatisticsContext', fromStoreVecOr0Ptr*`Vector Word64'&} -> `()'#}
{#fun unsafe StatisticsSetCostLimit as ^ {`StatisticsContext', `Word64'} -> `()'#}
{#fun unsafe StatisticsClear as ^ {`StatisticsContext'} -> `()'#}

-- AST Module
{#fun unsafe ASTModuleListImportsLength as ^ {`ASTModuleContext'} -> `Word32'#}
{#fun unsafe ASTModuleListImports as astModuleListImports_ {`ASTModuleContext', fromMutIOVecOr0Ptr*`IOVector ImportTypeContext'&} -> `Word32'#}
{#fun unsafe ASTModuleListExportsLength as ^ {`ASTModuleContext'} -> `Word32'#}
{#fun unsafe ASTModuleListExports as astModuleListExports_ {`ASTModuleContext', fromMutIOVecOr0Ptr*`IOVector ExportTypeContext'&} -> `Word32'#}

-- * Function
{#fun unsafe FunctionTypeCreate as ^ {fromStoreVecOr0Ptr*`Vector ValType'&, fromStoreVecOr0Ptr*`Vector ValType'&} -> `FunctionTypeContext'#}
{#fun unsafe FunctionTypeGetParametersLength as ^ {`FunctionTypeContext'} -> `Word32'#}
{#fun unsafe FunctionTypeGetParameters as functionTypeGetParameters_ {`FunctionTypeContext', fromMutIOVecOr0Ptr*`IOVector ValType'&} -> `Word32'#}


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

fromVecOr0Ptr :: (a -> IO (Ptr c)) -> V.Vector a -> ((Ptr n, CUInt) -> IO b) -> IO b
fromVecOr0Ptr getPtr v f
  | V.null v = f (nullPtr, 0)
  | otherwise = do
      ptrs <- VSM.generateM (fromIntegral $ V.length v) (getPtr . V.unsafeIndex v)
      r <- fromMutIOVecOr0Ptr ptrs f
      VSM.mapM_ free ptrs
      pure r

fromVecStringOr0Ptr :: V.Vector String -> ((Ptr n, CUInt) -> IO b) -> IO b
fromVecStringOr0Ptr = fromVecOr0Ptr newCString

fromMutIOVecOr0Ptr :: (Storable a) => IOVector a -> ((Ptr n, CUInt) -> IO b) -> IO b
fromMutIOVecOr0Ptr v f
  | VSM.null v = f (nullPtr, 0)
  | otherwise = VSM.unsafeWith v $ \p -> f (castPtr p, fromIntegral $ VSM.length v)

newtype ViaFromEnum t = ViaFromEnum {getHsEnumTy :: t}

instance Enum t => Storable (ViaFromEnum t) where
  sizeOf = sizeOf . fromEnum . getHsEnumTy
  alignment = alignment . fromEnum . getHsEnumTy
  peek = fmap (ViaFromEnum . toEnum) . peek @Int . castPtr 
  poke p v = poke @Int (castPtr p) (fromEnum $ getHsEnumTy v)
-- * Function Type
{#fun unsafe FunctionTypeGetReturnsLength as ^ {`FunctionTypeContext'} -> `Word32'#}
{#fun unsafe FunctionTypeGetReturns as functionTypeGetReturns_ {`FunctionTypeContext', fromMutIOVecOr0Ptr*`IOVector ValType'&} -> `Word32'#}

functionTypeGetReturns :: FunctionTypeContext -> Word32 -> IO (Vector ValType)
functionTypeGetReturns fcxt buffLen = do
  v <- VSM.new (fromIntegral buffLen)
  len <- functionTypeGetReturns_ fcxt v
  VS.unsafeFreeze $ VSM.slice 0 (fromIntegral len) v

noFinalizer :: (Coercible (ForeignPtr t) t) => Ptr t -> IO t
noFinalizer = coerce . newForeignPtr_

-- Table Type
{#fun unsafe TableTypeCreate as ^ {`RefType',%`Limit'} -> `TableTypeContext' #}
{#fun unsafe TableTypeGetRefType as ^ {`TableTypeContext'} -> `RefType'#}      
{#fun unsafe TableTypeGetLimitOut as tableTypeGetLimit {+,`TableTypeContext'} -> `Limit'#}
-- Memory Type
{#fun unsafe MemoryTypeCreate as ^ {%`Limit'} -> `MemoryTypeContext'#}  
{#fun unsafe MemoryTypeGetLimitOut as memoryTypeGetLimit {+,`MemoryTypeContext'} -> `Limit'#} 

-- Global Type
{#fun unsafe GlobalTypeCreate as ^ {`ValType',`Mutability'} -> `GlobalTypeContext'#} 
{#fun unsafe GlobalTypeGetMutability as ^ {`GlobalTypeContext'} -> `Mutability'#}

-- Import Type
{#fun unsafe ImportTypeGetModuleNameOut as importTypeGetModuleName {+,`ImportTypeContext'} -> `WasmString'#}
{#fun unsafe ImportTypeGetExternalNameOut as importTypeGetExternalName {+,`ImportTypeContext'} -> `WasmString'#}
{#fun unsafe ImportTypeGetFunctionType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `FunctionTypeContext'noFinalizer*#} 
{#fun unsafe ImportTypeGetTableType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `TableTypeContext'#} 
{#fun unsafe ImportTypeGetMemoryType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `MemoryTypeContext'#} 
{#fun unsafe ImportTypeGetGlobalType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `GlobalTypeContext'#} 

-- Export Type
{#fun unsafe ExportTypeGetExternalType as ^ {`ExportTypeContext'} -> `ExternalType'#} 
{#fun unsafe ExportTypeGetExternalNameOut as exportTypeGetExternalName {+,`ExportTypeContext'} -> `WasmString'#} 
{#fun unsafe ExportTypeGetFunctionType as ^ {`ASTModuleContext',`ExportTypeContext'} -> `FunctionTypeContext'#} 
{#fun unsafe ExportTypeGetTableType as ^ {`ASTModuleContext',`ExportTypeContext'} -> `TableTypeContext'#}
{#fun unsafe ExportTypeGetMemoryType as ^ {`ASTModuleContext',`ExportTypeContext'} -> `MemoryTypeContext'#}
{#fun unsafe ExportTypeGetGlobalType as ^ {`ASTModuleContext',`ExportTypeContext'} -> `GlobalTypeContext'#}

-- AOT Compiler
{#fun unsafe CompilerCreate as ^ {`ConfigureContext'} -> `CompilerContext'#}
{#fun unsafe CompilerCompileOut as compilerCompile {+,`CompilerContext',`String',`String'} -> `WasmResult'#} 
{#fun unsafe CompilerCompileFromBufferOut as compilerCompileFromBuffer {+,`CompilerContext',`Word8',`Word64',`String'} -> `WasmResult'#} 

-- Loader
{#fun unsafe LoaderCreate as ^ {`ConfigureContext'} -> `LoaderContext'#}
{#fun unsafe LoaderParseFromFileOut as loaderParseFromFile_ {+,`LoaderContext',id`Ptr (Ptr ASTModuleContext)',`String'} -> `WasmResult'#}
{#fun unsafe LoaderParseFromBufferOut as loaderParseFromBuffer_ {+, `LoaderContext',id`Ptr (Ptr ASTModuleContext)',useAsPtrCUCharLenBS*`ByteString'&} -> `WasmResult'#}

-- Validator
{#fun unsafe ValidatorCreate as ^ {`ConfigureContext'} -> `ValidatorContext'#}
{#fun unsafe ValidatorValidateOut as validatorValidate {+,`ValidatorContext',`ASTModuleContext'} -> `WasmResult'#}

-- Executor
{#fun unsafe ExecutorCreate as ^ {`ConfigureContext',`StatisticsContext'} -> `ExecutorContext'#}
{#fun unsafe ExecutorInstantiateOut as executorInstantiate {+,`ExecutorContext',alloca-`ModuleInstanceContext'peekOutPtr*,`StoreContext',`ASTModuleContext'} -> `WasmResult'#}
{#fun unsafe ExecutorRegisterOut as executorRegister {+,`ExecutorContext',alloca-`ModuleInstanceContext'peekOutPtr*,`StoreContext',`ASTModuleContext',`WasmString'} -> `WasmResult'#}
{#fun unsafe ExecutorRegisterImportOut as executorRegisterImport {+,`ExecutorContext',`StoreContext',`ModuleInstanceContext'} -> `WasmResult'#}
{#fun unsafe ExecutorInvokeOut as executorInvoke {+,`ExecutorContext',`FunctionInstanceContext',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe ExecutorAsyncInvokeOut as executorAsyncInvoke {`ExecutorContext',`FunctionInstanceContext',`WasmVal',`Word32'} -> `Async'#}

peekOutPtr :: (Coercible (ForeignPtr t) t, HasFinalizer t) => Ptr (Ptr t) -> IO t
peekOutPtr pout = do
  pres <- peek pout
  fmap coerce $ newForeignPtr getFinalizer pres


-- Store
{#fun unsafe StoreCreate as ^ {} -> `StoreContext'#} 
{#fun unsafe StoreFindModule as ^ {`StoreContext',%`WasmString'} -> `ModuleInstanceContext'#}
{#fun unsafe StoreListModuleLength as ^ {`StoreContext'} -> `Word32'#}
{#fun unsafe StoreListModule as ^ {`StoreContext',`WasmString',`Word32'} -> `Word32'#}

-- Module Instance
{#fun unsafe ModuleInstanceCreate as ^ {%`WasmString'} -> `ModuleInstanceContext'#} 
-- TODO:
-- {#fun unsafe ModuleInstanceCreateWithData as ^ {%`WasmString',`Ptr ()',`(FunPtr (Ptr () -> IO())'} -> `ModuleInstanceContext'#} -- Function as an argument
{#fun unsafe ModuleInstanceCreateWASI as ^ {fromVecStringOr0Ptr*`V.Vector String'&,fromVecStringOr0Ptr*`V.Vector String'&,fromVecStringOr0Ptr*`V.Vector String'&} -> `ModuleInstanceContext'#}
{#fun unsafe ModuleInstanceInitWASI as ^ {`ModuleInstanceContext',fromVecStringOr0Ptr*`V.Vector String'&,fromVecStringOr0Ptr*`V.Vector String'&,fromVecStringOr0Ptr*`V.Vector String'&} -> `()'#}
{#fun unsafe ModuleInstanceWASIGetExitCode as ^ {`ModuleInstanceContext'} -> `Word32'#}
{#fun unsafe ModuleInstanceWASIGetNativeHandler as ^ {`ModuleInstanceContext',`Word32',`Word64'} -> `Word32'#} -- TODO: word*
{#fun unsafe ModuleInstanceInitWasmEdgeProcess as ^ {fromVecStringOr0Ptr*`V.Vector String'&,`Bool'} -> `()'#}
{#fun unsafe ModuleInstanceGetModuleNameOut as moduleInstanceGetModuleName {+,`ModuleInstanceContext'} -> `WasmString'#}
{#fun unsafe ModuleInstanceGetHostData as ^ {`ModuleInstanceContext'} -> `()'#} -- TODO: void*
{#fun unsafe ModuleInstanceFindFunction as ^ {`ModuleInstanceContext',%`WasmString'} -> `FunctionInstanceContext'#}
{#fun unsafe ModuleInstanceFindTable as ^ {`ModuleInstanceContext',%`WasmString'} -> `TableInstanceContext'#}
{#fun unsafe ModuleInstanceFindMemory as ^ {`ModuleInstanceContext',%`WasmString'} -> `MemoryInstanceContext'#}
{#fun unsafe ModuleInstanceFindGlobal as ^ {`ModuleInstanceContext',%`WasmString'} -> `GlobalInstanceContext'#}
{#fun unsafe ModuleInstanceListFunctionLength as ^ {`ModuleInstanceContext'} -> `Word32'#}
{#fun unsafe ModuleInstanceListFunction as ^ {`ModuleInstanceContext',`WasmString',`Word32'} -> `Word32'#}
{#fun unsafe ModuleInstanceListTableLength as ^ {`ModuleInstanceContext'} -> `Word32'#} 
{#fun unsafe ModuleInstanceListTable as ^ {`ModuleInstanceContext',`WasmString',`Word32'} -> `Word32'#}
{#fun unsafe ModuleInstanceListMemoryLength as ^ {`ModuleInstanceContext'} -> `Word32'#} 
{#fun unsafe ModuleInstanceListMemory as ^ {`ModuleInstanceContext',`WasmString',`Word32'} -> `Word32'#} 
{#fun unsafe ModuleInstanceListGlobalLength as ^ {`ModuleInstanceContext'} -> `Word32'#} 
{#fun unsafe ModuleInstanceListGlobal as ^ {`ModuleInstanceContext',`WasmString',`Word32'} -> `Word32'#} 
{#fun unsafe ModuleInstanceAddFunction as ^ {`ModuleInstanceContext',%`WasmString',`FunctionInstanceContext'} -> `()'#}
{#fun unsafe ModuleInstanceAddTable as ^ {`ModuleInstanceContext',%`WasmString',`TableInstanceContext'} -> `()'#}
{#fun unsafe ModuleInstanceAddMemory as ^ {`ModuleInstanceContext',%`WasmString',`MemoryInstanceContext'} -> `()'#}
{#fun unsafe ModuleInstanceAddGlobal as ^ {`ModuleInstanceContext',%`WasmString',`GlobalInstanceContext'} -> `()'#}

-- Function Instance
{-
typedef WasmEdge_Result (*WasmEdge_HostFunc_t)(
    void *Data, const WasmEdge_CallingFrameContext *CallFrameCxt,
    const WasmEdge_Value *Params, WasmEdge_Value *Returns);
-}
{-
typedef WasmEdge_Result (*WasmEdge_WrapFunc_t)(
    void *This, void *Data, const WasmEdge_CallingFrameContext *CallFrameCxt,
    const WasmEdge_Value *Params, const uint32_t ParamLen,
    WasmEdge_Value *Returns, const uint32_t ReturnLen);
-}
{#pointer HostFunc_t as WasmHostFunc newtype #}
-- TODO:
-- {#fun unsafe FunctionInstanceCreate as ^ {`FunctionTypeContext',`HostFunc_t',`void *',`Word64'} -> `FunctionTypeContext'#}  --some random typedef
-- {#fun unsafe FunctionInstanceCreateBinding as ^ {`FunctionTypeContext',`WrapFunc_t',`void *',`void *',`Word64'} -> `FunctionInstanceContext'#} -- some random typedef
{#fun unsafe FunctionInstanceGetFunctionType as ^ {`FunctionInstanceContext'} -> `FunctionTypeContext'#}

-- Table Instance
{#fun unsafe TableInstanceCreate as ^ {`TableTypeContext'} -> `TableInstanceContext'#}
{#fun unsafe TableInstanceGetTableType as ^ {`TableInstanceContext'} -> `TableTypeContext'#}
{#fun unsafe TableInstanceGetDataOut as tableInstanceGetData  {+,`TableInstanceContext',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe TableInstanceSetDataOut as tableInstanceSetData  {+,`TableInstanceContext',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe TableInstanceGetSize as ^ {`TableInstanceContext'} -> `Word32'#} 
{#fun unsafe TableInstanceGrowOut as tableInstanceGrow {+,`TableInstanceContext',`Word32'} -> `WasmResult'#}

-- Memory Instance
{#fun unsafe MemoryInstanceCreate as ^ {`MemoryTypeContext'} -> `MemoryInstanceContext'#} 
{#fun unsafe MemoryInstanceGetMemoryType as ^ {`MemoryInstanceContext'} -> `MemoryTypeContext'#} 
{#fun unsafe MemoryInstanceGetDataOut as memoryInstanceGetData {+,`MemoryInstanceContext',`Word8',`Word32',`Word32'} -> `WasmResult'#} 
-- TODO:
-- {#fun unsafe MemoryInstanceSetDataOut as memoryInstanceSetData {+,`MemoryInstanceContext',`Word32',fromStoreVecOr0Ptr*`Vector Word8'&} -> `WasmResult'#} -- use wrapper with offset before data
-- {#fun unsafe MemoryInstanceGetPointer as ^ {`MemoryInstanceContext',`Word32',`Word32'} -> `Vector Word8'#} -- Haskell type: Ptr Word8 C type      : (IO (C2HSImp.Ptr C2HSImp.CUChar))
-- {#fun unsafe MemoryInstanceGetPointerConst as ^ {`MemoryInstanceContext',`Word32',`Word32'} -> `Word8'#} --Haskell type: Ptr Word8 C type      : (IO (C2HSImp.Ptr C2HSImp.CUChar))
{#fun unsafe MemoryInstanceGetPageSize as ^ {`MemoryInstanceContext'} -> `Word32'#} 
{#fun unsafe MemoryInstanceGrowPageOut as memoryInstanceGrowPage {+,`MemoryInstanceContext',`Word32'} -> `WasmResult'#} 

-- Global Instance
{#fun unsafe GlobalInstanceCreateOut as globalInstanceCreate {`GlobalTypeContext',`WasmVal'} -> `GlobalInstanceContext'#}
{#fun unsafe GlobalInstanceGetGlobalType as ^ {`GlobalInstanceContext'} -> `GlobalTypeContext'#} 
{#fun unsafe GlobalInstanceGetValueOut as globalInstanceGetValue  {+,`GlobalInstanceContext'} -> `WasmVal'#} --How to return wasmvalue 
{#fun unsafe GlobalInstanceSetValueOut as globalInstanceSetValue {`GlobalInstanceContext',`WasmVal'} -> `()'#} 

-- Calling Frame
{#fun unsafe CallingFrameGetExecutor as ^ {`CallingFrameContext'} -> `ExecutorContext'#}
{#fun unsafe CallingFrameGetModuleInstance as ^ {`CallingFrameContext'} -> `ModuleInstanceContext'#}
{#fun unsafe CallingFrameGetMemoryInstance as ^ {`CallingFrameContext',`Word32'} -> `MemoryInstanceContext'#}

-- Async
{#fun unsafe AsyncWait as ^ {`Async'} -> `()'#}
{#fun unsafe AsyncWaitFor as ^ {`Async',`Word64'} -> `Bool'#}
{#fun unsafe AsyncCancel as ^ {`Async'} -> `()'#}
{#fun unsafe AsyncGetReturnsLength as ^ {`Async'} -> `Word32'#}
{#fun unsafe AsyncGetOut as asyncGet {+,`Async',`WasmVal',`Word32'} -> `WasmResult'#}

-- VM
{#fun unsafe VMCreate as ^ {`ConfigureContext',`StoreContext'} -> `VMContext'#}
{#fun unsafe VMRegisterModuleFromFileOut as vMRegisterModuleFromFile {+,`VMContext',%`WasmString',`String'} -> `WasmResult'#}
{#fun unsafe VMRunWasmFromFileOut as vMRunWasmFromFile {+,`VMContext',`String',%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe VMRunWasmFromBufferOut as vMRunWasmFromBuffer {+,`VMContext',fromStoreVecOr0Ptr*`Vector Word8'& ,%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#} 
{#fun unsafe VMRunWasmFromASTModuleOut as vMRunWasmFromASTModule {+,`VMContext',`ASTModuleContext',%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe VMAsyncRunWasmFromFileOut as vMAsyncRunWasmFromFile {`VMContext',`String',%`WasmString',`WasmVal',`Word32'} -> `Async'#}
{#fun unsafe VMAsyncRunWasmFromASTModuleOut as vMAsyncRunWasmFromASTModule {`VMContext',`ASTModuleContext',%`WasmString',`WasmVal',`Word32'} -> `Async'#}
{#fun unsafe VMAsyncRunWasmFromBufferOut as vMAsyncRunWasmFromBuffer {`VMContext',fromStoreVecOr0Ptr*`Vector Word64'&, %`WasmString',`WasmVal',`Word32'} -> `Async'#}
{#fun unsafe VMRegisterModuleFromBufferOut as vMRegisterModuleFromBuffer {+,`VMContext',%`WasmString',fromStoreVecOr0Ptr*`Vector Word8'& } -> `WasmResult'#}
{#fun unsafe VMRegisterModuleFromASTModuleOut as vMRegisterModuleFromASTModule {+,`VMContext',%`WasmString',`ASTModuleContext'} -> `WasmResult'#}
{#fun unsafe VMRegisterModuleFromImportOut as vMRegisterModuleFromImport {+,`VMContext',`ModuleInstanceContext'} -> `WasmResult'#}
{#fun unsafe VMLoadWasmFromFileOut as vMLoadWasmFromFile {+,`VMContext',`String'} -> `WasmResult'#}
{#fun unsafe VMLoadWasmFromBufferOut as vMLoadWasmFromBuffer {+,`VMContext',fromStoreVecOr0Ptr*`Vector Word8'& } -> `WasmResult'#}
{#fun unsafe VMLoadWasmFromASTModuleOut as vMLoadWasmFromASTModule {+,`VMContext',`ASTModuleContext'} -> `WasmResult'#}
{#fun unsafe VMValidateOut as vMValidate  {+,`VMContext'} -> `WasmResult'#}
{#fun unsafe VMInstantiateOut as vMInstantiate  {+,`VMContext'} -> `WasmResult'#}
{#fun unsafe VMExecuteOut as vMExecute {+,`VMContext',%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe VMExecuteRegisteredOut as vMExecuteRegistered {+,`VMContext',%`WasmString',%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe VMAsyncExecuteOut as vMAsyncExecute {`VMContext',%`WasmString',`WasmVal',`Word32'} -> `Async'#}
{#fun unsafe VMAsyncExecuteRegisteredOut as vMAsyncExecuteRegistered {`VMContext',%`WasmString',%`WasmString',`WasmVal',`Word32'} -> `Async'#} 
{#fun unsafe VMGetFunctionType as ^ {`VMContext',%`WasmString'} -> `FunctionTypeContext'#}
{#fun unsafe VMGetFunctionTypeRegistered as ^ {`VMContext',%`WasmString',%`WasmString'} -> `FunctionTypeContext'#}
{#fun unsafe VMCleanup as ^ {`VMContext'} -> `()'#} 
{#fun unsafe VMGetFunctionListLength as ^ {`VMContext'} -> `Word32'#} 
{#fun unsafe VMGetFunctionList as ^ {`VMContext',`WasmString',alloca-`FunctionTypeContext'peekOutPtr*,`Word32'} -> `Word32'#} --Double pointer, Expected FunctionTypeContext Actual Ptr FunctionTypeContext
{#fun unsafe VMGetImportModuleContext as ^ {`VMContext',`HostRegistration'} -> `ModuleInstanceContext'#} 
{#fun unsafe VMGetActiveModule as ^ {`VMContext'} -> `ModuleInstanceContext'#} 
{#fun unsafe VMGetRegisteredModule as ^ {`VMContext',%`WasmString'} -> `ModuleInstanceContext'#}
{#fun unsafe VMListRegisteredModuleLength as ^ {`VMContext'} -> `Word32'#} 
{#fun unsafe VMListRegisteredModule as ^ {`VMContext',`WasmString',`Word32'} -> `Word32'#} 
{#fun unsafe VMGetStoreContext as ^ {`VMContext'} -> `StoreContext'#} 
{#fun unsafe VMGetLoaderContext as ^ {`VMContext'} -> `LoaderContext'#} 
{#fun unsafe VMGetValidatorContext as ^ {`VMContext'} -> `ValidatorContext'#} 
{#fun unsafe VMGetExecutorContext as ^ {`VMContext'} -> `ExecutorContext'#} 
{#fun unsafe VMGetStatisticsContext as ^ {`VMContext'} -> `StatisticsContext'#} 

-- Driver
-- {#fun unsafe Driver_Compiler as ^ {`Int',`Ptr String'} -> `Int'#} -- Const Char* 
-- {#fun unsafe Driver_Tool as ^ {`Int',`String'} -> `Int'#} -- Const Char* 
-- {#fun unsafe Driver_UniTool as ^ {`Int',`String'} -> `Int'#} -- Const Char* 

-- Plugin Function
{#fun unsafe PluginLoadWithDefaultPaths as ^ {} -> `()'#} 
{#fun unsafe PluginLoadFromPath as ^ {`String'} -> `()'#}
{#fun unsafe PluginListPluginsLength as ^ {} -> `Word32'#} 
{#fun unsafe PluginListPlugins as ^ {`WasmString',`Word32'} -> `Word32'#} 
{#fun unsafe PluginFind as ^ {%`WasmString'} -> `PluginContext'#} 
{#fun unsafe PluginGetPluginNameOut as pluginGetPluginName {+,`PluginContext'} -> `WasmString'#} 
{#fun unsafe PluginListModuleLength as ^ {`PluginContext'} -> `Word32'#} 
{#fun unsafe PluginListModule as ^ {`PluginContext',`WasmString',`Word32'} -> `Word32'#} 
{#fun unsafe PluginCreateModule as ^ {`PluginContext',%`WasmString'} -> `ModuleInstanceContext'#} 
{#fun unsafe Plugin_GetDescriptor as ^ {} -> `PluginDescriptor'#} 
