{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module WasmEdge.Internal.FFI.ValueTypes
  ( valueGenI32
  , wasmStringEq
  , wasmStringLength
  , toText
  , mkStringFromBytes
  , stringCopy
  , configureAddHostRegistration
  , logSetErrorLevel
  , logSetDebugLevel
  , finalize
  , HasFinalizer
  , ConfigureContext
  , ProgramOptionType (..)
  , Proposal (..)
  , HostRegistration (..)
  , CompilerOptimizationLevel (..)
  , CompilerOutputFormat (..)
  , ErrCategory (..)
  , ErrCode (..)
  , ValType (..)
  , NumType (..)
  , RefType (..)
  , ExternalType (..)
  , Mutability (..)
  , WasmString
  , WasmVal (WasmInt32, WasmInt64, WasmFloat, WasmDouble, WasmInt128)
#if TESTONLY
  , testonly_accquire
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
-- import GHC.Ptr
import System.IO.Unsafe
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Coerce
import Control.Arrow ((&&&))
import Data.WideWord.Int128
#if TESTONLY
import Data.Unique
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.MVar
import GHC.Stack
#endif

-- import Data.Vector.Storable.Mutable (IOVector)
-- import qualified Data.Vector.Storable.Mutable as VSM

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

void ImportTypeGetModuleNameOut(WasmEdge_String* strOut,WasmEdge_ImportTypeContext* Ctx){ *strOut = WasmEdge_ImportTypeGetModuleName(Ctx); }
void ImportTypeGetExternalNameOut(WasmEdge_String* strOut,WasmEdge_ImportTypeContext* Ctx){ *strOut = WasmEdge_ImportTypeGetExternalName(Ctx); }
void ExportTypeGetExternalNameOut(WasmEdge_String* strOut,WasmEdge_ExportTypeContext* Ctx){ *strOut = WasmEdge_ExportTypeGetExternalName(Ctx); }
void CompilerCompileOut(WasmEdge_Result* resOut,WasmEdge_CompilerContext* Ctx,const char* InPath,const char* OutPath){ *resOut = WasmEdge_CompilerCompile(Ctx,InPath,OutPath); }
void CompilerCompileFromBufferOut(WasmEdge_Result* resOut,WasmEdge_CompilerContext* Ctx,const uint8_t *InBuffer,const uint64_t InBufferLen,const char *OutPath){ 
  *resOut = WasmEdge_CompilerCompileFromBuffer(Ctx,InBuffer,InBufferLen,OutPath); 
  }
void ValidatorValidateOut(WasmEdge_Result* resOut,WasmEdge_ValidatorContext* Ctx,const WasmEdge_ASTModuleContext *ASTCxt){ *resOut = WasmEdge_ValidatorValidate(Ctx,ASTCxt); }
//void TableTypeGetLimitOut(WasmEdge_Limit* limOut,const WasmEdge_TableTypeContext *Cxt){ *limOut = WasmEdge_TableTypeGetLimit(Cxt); }
void ExecutorRegisterImportOut(WasmEdge_Result* resOut, WasmEdge_ExecutorContext *Cxt, WasmEdge_StoreContext *StoreCxt,const WasmEdge_ModuleInstanceContext *ImportCxt){ 
*resOut = WasmEdge_ExecutorRegisterImport(Cxt,StoreCxt,ImportCxt); }
void ExecutorInvokeOut(WasmEdge_Result *resOut, WasmEdge_ExecutorContext *Cxt,const WasmEdge_FunctionInstanceContext *FuncCxt,const WasmVal *v1, const uint32_t ParamLen,WasmVal *v2, const uint32_t ReturnLen){
    WasmEdge_Value Params = {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
    WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
    *resOut = WasmEdge_ExecutorInvoke (Cxt,FuncCxt,&Params,ParamLen,&Returns,ReturnLen); //Is this correct??
}
WasmEdge_Async *ExecutorAsyncInvokeOut(WasmEdge_ExecutorContext *Cxt,const WasmEdge_FunctionInstanceContext *FuncCxt,const WasmVal *v,const uint32_t ParamLen){
    WasmEdge_Value Params = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
    return WasmEdge_ExecutorAsyncInvoke(Cxt,FuncCxt,&Params,ParamLen);
}
void ModuleInstanceGetModuleNameOut(WasmEdge_String* strOut,WasmEdge_ModuleInstanceContext* Ctx){ *strOut = WasmEdge_ModuleInstanceGetModuleName(Ctx); }
void TableInstanceGetDataOut(WasmEdge_Result* resOut,const WasmEdge_TableInstanceContext *Cxt,WasmVal *v, const uint32_t Offset){
   WasmEdge_Value Data = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  *resOut = WasmEdge_TableInstanceGetData(Cxt,&Data,Offset);
}
void TableInstanceSetDataOut(WasmEdge_Result *resOut,WasmEdge_TableInstanceContext *Cxt,WasmVal *v, const uint32_t Offset){
  WasmEdge_Value Data = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  *resOut = WasmEdge_TableInstanceSetData(Cxt,Data,Offset);
}
void TableInstanceGrowOut(WasmEdge_Result* resOut,WasmEdge_TableInstanceContext* Ctx,const uint32_t Size){ *resOut = WasmEdge_TableInstanceGrow(Ctx,Size); }
void MemoryInstanceGetDataOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext* Ctx,uint8_t *Data, const uint32_t Offset,const uint32_t Length){ 
*resOut = WasmEdge_MemoryInstanceGetData(Ctx,Data,Offset,Length); }
void MemoryInstanceSetDataOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext* Ctx,uint8_t *Data, const uint32_t Offset,const uint32_t Length){ 
*resOut = WasmEdge_MemoryInstanceSetData(Ctx,Data,Offset,Length); }
void MemoryInstanceGrowPageOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext *Cxt,const uint32_t Page){ *resOut = WasmEdge_MemoryInstanceGrowPage(Cxt,Page); }
WasmEdge_GlobalInstanceContext* GlobalInstanceCreateOut (const WasmEdge_GlobalTypeContext *GlobType,WasmVal* v){
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_GlobalInstanceCreate(GlobType,val); 
}
void GlobalInstanceSetValueOut(WasmEdge_GlobalInstanceContext *Cxt,const WasmVal *v){
  WasmEdge_Value val = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  WasmEdge_GlobalInstanceSetValue(Cxt,val);
}
void AsyncGetOut(WasmEdge_Result *resOut,const WasmEdge_Async *Cxt, WasmVal *v,const uint32_t ReturnLen){
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  *resOut = WasmEdge_AsyncGet(Cxt,&Returns,ReturnLen);
}
void VMRunWasmFromFileOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const char *Path, const WasmEdge_String FuncName,const WasmVal *v1, const uint32_t ParamLen,WasmVal *v2, const uint32_t ReturnLen){
  WasmEdge_Value Params= {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMRunWasmFromFile(Cxt,Path,FuncName,&Params,ParamLen,&Returns,ReturnLen);
}
void VMRunWasmFromASTModuleOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_ASTModuleContext *ASTCxt,const WasmEdge_String FuncName, const WasmVal *v1,const uint32_t ParamLen, WasmVal *v2, const uint32_t ReturnLen){
  WasmEdge_Value Params= {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMRunWasmFromASTModule(Cxt,ASTCxt,FuncName,&Params,ParamLen,&Returns,ReturnLen); 
}
WasmEdge_Async *VMAsyncRunWasmFromFileOut(WasmEdge_VMContext *Cxt, const char *Path, const WasmEdge_String FuncName,const WasmVal *v, const uint32_t ParamLen){
  WasmEdge_Value Params= {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_VMAsyncRunWasmFromFile(Cxt,Path,FuncName,&Params,ParamLen);
}
WasmEdge_Async *VMAsyncRunWasmFromASTModuleOut(WasmEdge_VMContext *Cxt,const WasmEdge_ASTModuleContext *ASTCxt,const WasmEdge_String FuncName,const WasmVal *v,const uint32_t ParamLen){
  WasmEdge_Value Params= {.Value = pack_uint128_t(v->Val), .Type = v->Type};
  return WasmEdge_VMAsyncRunWasmFromASTModule(Cxt,ASTCxt,FuncName,&Params,ParamLen);
}
void VMExecuteOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_String FuncName,const WasmVal *v1, const uint32_t ParamLen,WasmVal *v2, const uint32_t ReturnLen){
  WasmEdge_Value Params = {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMExecute(Cxt,FuncName,&Params,ParamLen,&Returns,ReturnLen);
}
void VMExecuteRegisteredOut(WasmEdge_Result *resOut,WasmEdge_VMContext *Cxt, const WasmEdge_String ModuleName,const WasmEdge_String FuncName, const WasmVal *v1,const uint32_t ParamLen, WasmVal *v2, const uint32_t ReturnLen){
   WasmEdge_Value Params = {.Value = pack_uint128_t(v1->Val), .Type = v1->Type};
  WasmEdge_Value Returns = {.Value = pack_uint128_t(v2->Val), .Type = v2->Type};
  *resOut = WasmEdge_VMExecuteRegistered(Cxt,ModuleName,FuncName,&Params,ParamLen,&Returns,ReturnLen); 
}
WasmEdge_Async *VMAsyncExecuteOut(WasmEdge_VMContext *Cxt, const WasmEdge_String FuncName,const WasmVal *v, const uint32_t ParamLen){
   WasmEdge_Value Params = {.Value = pack_uint128_t(v->Val), .Type = v->Type};
   return WasmEdge_VMAsyncExecute(Cxt,FuncName,&Params,ParamLen);
}
WasmEdge_Async* VMAsyncExecuteRegisteredOut(WasmEdge_VMContext *Cxt, const WasmEdge_String ModuleName,const WasmEdge_String FuncName, const WasmVal *v,const uint32_t ParamLen){
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
#endc


{#pointer *WasmVal as WasmVal foreign newtype #}
{#pointer *WasmEdge_String as WasmString foreign finalizer StringDeleteByPtr as deleteString newtype #}
instance HasFinalizer WasmString

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

-- Value

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

{-# COMPLETE WasmInt32, WasmInt64, WasmFloat, WasmDouble, WasmInt128 #-}

instance Show WasmVal where
  show = \case
    WasmInt32 v -> show v
    WasmInt64 v -> show v
    WasmFloat v -> show v
    WasmDouble v -> show v
    WasmInt128 v -> show v

getValType :: WasmVal -> ValType
getValType v = unsafePerformIO $ withWasmVal v (fmap cToEnum . {#get WasmVal.Type #})

{#fun pure unsafe ValueGenI32 as valueGenI32 {+, `Int32'} -> `WasmVal' #}
{#fun pure unsafe ValueGetI32 as valueGetI32 {`WasmVal'} -> `Int32' #}

{#fun pure unsafe ValueGenI64 as valueGenI64 {+, `Int64'} -> `WasmVal' #}
{#fun pure unsafe ValueGetI64 as valueGetI64 {`WasmVal'} -> `Int64' #}

{#fun pure unsafe ValueGenF32 as valueGenF32 {+, `Float'} -> `WasmVal' #}
{#fun pure unsafe ValueGetF32 as valueGetF32 {`WasmVal'} -> `Float' #}

{#fun pure unsafe ValueGenF64 as valueGenF64 {+, `Double'} -> `WasmVal' #}
{#fun pure unsafe ValueGetF64 as valueGetF64 {`WasmVal'} -> `Double' #}

{#fun pure unsafe ValueGenV128 as valueGenV128 {+, fromI128*`Int128'} -> `WasmVal' #}
{#fun pure unsafe ValueGetV128 as valueGetV128 {`WasmVal', fromI128Alloc-`Int128'toI128*} -> `()' #}

{#fun unsafe StringCreateByBufferOut as mkStringFromBytesIO {+, useAsCStringLenBS*`ByteString'& } -> `WasmString' #}
{#fun pure unsafe StringWrapOut as stringWrap {+, useAsCStringLenBS*`ByteString'&} -> `WasmString' #}
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
  getFinalizer :: t -> IO ()
  getFinalizer t = finalizeForeignPtr @t (coerce t)
  
finalize :: HasFinalizer t => t -> IO ()
finalize = getFinalizer

useAsCStringLenBS :: ByteString -> ((CString, CUInt) -> IO a) -> IO a
useAsCStringLenBS bs f = BS.useAsCStringLen bs (\strLen -> f (fromIntegral <$> strLen))

_packCStringLenBS :: CString -> CUInt -> IO ByteString
_packCStringLenBS cstr len = BS.packCStringLen (cstr, fromIntegral len)

packCStringBS :: CString -> IO ByteString
packCStringBS cstr = BS.packCString cstr

memBuffIn :: MemBuff -> ((Ptr CChar, CUInt) -> IO a) -> IO a
memBuffIn mem f = withForeignPtr (memBuff mem) $ \p -> f (p, fromIntegral (memBuffLen mem))

data MemBuff = MemBuff {memBuffLen :: Int, memBuff :: ForeignPtr CChar}

allocMemBuff :: Int -> IO MemBuff
allocMemBuff sz = MemBuff sz <$> mallocForeignPtrBytes sz

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

wasmStringLength :: WasmString -> IO Word32
wasmStringLength wstr = withWasmString wstr (fmap fromIntegral . {#get WasmEdge_String.Length #})

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

{#pointer *ConfigureContext as ^ foreign finalizer ConfigureDelete as ^ newtype #}
{#pointer *StatisticsContext as ^ foreign finalizer StatisticsDelete as ^ newtype #}
{#pointer *ASTModuleContext as ^ foreign finalizer ASTModuleDelete as ^ newtype #}
{#pointer *FunctionTypeContext as ^ foreign finalizer FunctionTypeDelete as ^ newtype #}
{#pointer *MemoryTypeContext as ^ foreign finalizer MemoryTypeDelete as ^ newtype #}
{#pointer *TableTypeContext as ^ foreign finalizer TableTypeDelete as ^ newtype #}
{#pointer *GlobalTypeContext as ^ foreign finalizer GlobalTypeDelete as ^ newtype #}
{#pointer *ImportTypeContext as ^ foreign newtype #}
{#pointer *ExportTypeContext as ^ foreign newtype #}
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

-- WASM Proposal C enumeration.
{#enum Proposal as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Host Module Registration C enumeration.
{#enum HostRegistration as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- AOT compiler optimization level C enumeration.
{#enum CompilerOptimizationLevel as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- AOT compiler output binary format C enumeration.
{#enum CompilerOutputFormat as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Error category C enumeration.
{#enum ErrCategory as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Error code C enumeration.
{#enum ErrCode as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Value type C enumeration.
{#enum ValType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq)
#}
deriving via ViaFromEnum ValType instance Storable ValType

-- WASM Number type C enumeration.
{#enum NumType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Reference type C enumeration.
{#enum RefType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Mutability C enumeration.
{#enum Mutability as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM External type C enumeration.
{#enum ExternalType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Configure
{#fun unsafe ConfigureCreate as ^ {} -> `ConfigureContext'#}
{#fun unsafe ConfigureAddProposal as ^ {`ConfigureContext',`Proposal'} -> `()'#}
{#fun unsafe ConfigureRemoveProposal as ^ {`ConfigureContext',`Proposal'} -> `()'#}
{#fun unsafe ConfigureHasProposal as ^ {`ConfigureContext',`Proposal'} -> `Bool'#}
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
-- TODO:
-- {#fun unsafe StatisticsSetCostTable as ^ {`StatisticsContext', `MemBuff'} -> `()'#}
{#fun unsafe StatisticsSetCostLimit as ^ {`StatisticsContext', `Word64'} -> `()'#}
{#fun unsafe StatisticsClear as ^ {`StatisticsContext'} -> `()'#}

-- AST Module
{#fun unsafe ASTModuleListImportsLength as ^ {`ASTModuleContext'} -> `Word32'#}
-- TODO:
-- {#fun unsafe ASTModuleListImports as ^ {`ASTModuleContext'} -> `()'#}
{#fun unsafe ASTModuleListExportsLength as ^ {`ASTModuleContext'} -> `Word32'#}
-- TODO:
-- {#fun unsafe ASTModuleListExports as ^ {`ASTModuleContext'} -> `()'#}

-- * Function
{#fun unsafe FunctionTypeCreate as ^ {fromIOVecOr0Ptr*`Vector ValType'&, fromIOVecOr0Ptr*`Vector ValType'&} -> `FunctionTypeContext'#}
{#fun unsafe FunctionTypeGetParametersLength as ^ {`FunctionTypeContext'} -> `Word32'#}
-- {#fun unsafe FunctionTypeGetParameters as ^ {`FunctionTypeContext', +} -> `(Vector ValType'#}


fromIOVecOr0Ptr :: Vector ValType -> ((Ptr CInt, CUInt) -> IO b) -> IO b
fromIOVecOr0Ptr v f
  | VS.null v = f (nullPtr, 0)
  | otherwise = VS.unsafeWith v $ \p -> f (castPtr p, fromIntegral $ VS.length v)

newtype ViaFromEnum t = ViaFromEnum {getHsEnumTy :: t}

instance Enum t => Storable (ViaFromEnum t) where
  sizeOf = sizeOf . fromEnum . getHsEnumTy
  alignment = alignment . fromEnum . getHsEnumTy
  peek = fmap (ViaFromEnum . toEnum) . peek @Int . castPtr 
  poke p v = poke @Int (castPtr p) (fromEnum $ getHsEnumTy v)
-- * Function Type
-- TODO:
{#fun unsafe FunctionTypeGetReturnsLength as ^ {`FunctionTypeContext'} -> `Word32'#}

-- Table Type
-- TODO:                                            
{#fun unsafe TableTypeCreate as ^ {`RefType',%`Limit'} -> `TableTypeContext' #}
{#fun unsafe TableTypeGetRefType as ^ {`TableTypeContext'} -> `RefType'#}      
-- TODO: 
-- {#fun unsafe TableTypeGetLimitOut as tableTypeGetLimit {+,`TableTypeContext'} -> `Limit'#} -- Wrote wrapper but giving some weird warning,Structure wrapping not allowed in return types..why?
-- Memory Type
{#fun unsafe MemoryTypeCreate as ^ {%`Limit'} -> `MemoryTypeContext'#}  
-- {#fun unsafe MemoryTypeGetLimit as ^ {`MemoryTypeContext'} -> `Limit'#} -- Expected Ptr Limit Actual Ptr ()

-- Global Type
-- TODO:
{#fun unsafe GlobalTypeCreate as ^ {`ValType',`Mutability'} -> `GlobalTypeContext'#}  -- ValType and Mutability enums are working some issue with Limit enum  -- ValType and Mutability enums are working some issue with Limit enum
{#fun unsafe GlobalTypeGetMutability as ^ {`GlobalTypeContext'} -> `Mutability'#}

-- Import Type
-- TODO:
{#fun unsafe ImportTypeGetModuleNameOut as importTypeGetModuleName {+,`ImportTypeContext'} -> `WasmString'#} -- It didn't work when I wrote as ImportTypeGetModuleName ... 'I' why?
{#fun unsafe ImportTypeGetExternalNameOut as importTypeGetExternalName {+,`ImportTypeContext'} -> `WasmString'#}
{#fun unsafe ImportTypeGetFunctionType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `FunctionTypeContext'#} 
{#fun unsafe ImportTypeGetTableType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `TableTypeContext'#} 
{#fun unsafe ImportTypeGetMemoryType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `MemoryTypeContext'#} 
{#fun unsafe ImportTypeGetGlobalType as ^ {`ASTModuleContext',`ImportTypeContext'} -> `GlobalTypeContext'#} 

-- Export Type
{#fun unsafe ExportTypeGetExternalType as ^ {`ExportTypeContext'} -> `ExternalType'#} 
-- TODO:
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
-- {#fun unsafe LoaderParseFromFileOut as loaderParseFromFile {+,`LoaderContext',`ASTModuleContext',`String'} -> `WasmResult'#} -- needs wrapper, double pointer for ASTModuleContext and something for const char*
-- {#fun unsafe LoaderParseFromBuffer as ^ {`LoaderContext',`ASTModuleContext',`ForeignPtr Word8',`Word32'} -> `WasmResult'#} -- needs wrapper, double pointer for ASTModuleContext 

-- Validator
{#fun unsafe ValidatorCreate as ^ {`ConfigureContext'} -> `ValidatorContext'#}
{#fun unsafe ValidatorValidateOut as validatorValidate {+,`ValidatorContext',`ASTModuleContext'} -> `WasmResult'#}

-- Executor
{#fun unsafe ExecutorCreate as ^ {`ConfigureContext',`StatisticsContext'} -> `ExecutorContext'#}
-- {#fun unsafe ExecutorInstantiate as ^ {`ExecutorContext',`ForeignPtr ModuleInstanceContext',`StoreContext',`ASTModuleContext'} -> `WasmResult'#} -- needs wrapper, double pointer
-- {#fun unsafe ExecutorRegister as ^ {`ExecutorContext',`ForeignPtr ModuleInstanceContext',`StoreContext',`ASTModuleContext',`WasmString'} -> `WasmResult'#} -- needs wrapper and WasmString substitute
{#fun unsafe ExecutorRegisterImportOut as executorRegisterImport {+,`ExecutorContext',`StoreContext',`ModuleInstanceContext'} -> `WasmResult'#}
{#fun unsafe ExecutorInvokeOut as executorInvoke {+,`ExecutorContext',`FunctionInstanceContext',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe ExecutorAsyncInvokeOut as executorAsyncInvoke {`ExecutorContext',`FunctionInstanceContext',`WasmVal',`Word32'} -> `Async'#} 

-- Store
{#fun unsafe StoreCreate as ^ {} -> `StoreContext'#} 
{#fun unsafe StoreFindModule as ^ {`StoreContext',%`WasmString'} -> `ModuleInstanceContext'#}
{#fun unsafe StoreListModuleLength as ^ {`StoreContext'} -> `Word32'#}
{#fun unsafe StoreListModule as ^ {`StoreContext',`WasmString',`Word32'} -> `Word32'#}

-- Module Instance
{#fun unsafe ModuleInstanceCreate as ^ {%`WasmString'} -> `ModuleInstanceContext'#} 
-- {#fun unsafe ModuleInstanceCreateWithData as ^ {%`WasmString',`Ptr ()',`void (*finalizer)(void *)'} -> `ModuleInstanceContext'#} -- WasmString and that void data
-- {#fun unsafe ModuleInstanceCreateWASI as ^ {`String',`Word32',`String',`String',`Word32',`String',`Word32'} -> `ModuleInstanceContext'#} -- char* const,uinst32_t 
-- {#fun unsafe ModuleInstanceInitWASI as ^ {`ModuleInstanceContext',`Const char *const',`Word32',`const char* const',`Word32',`Const char* const',`Word32'} -> `()'#} -- char* const,uinst32_t 
{#fun unsafe ModuleInstanceWASIGetExitCode as ^ {`ModuleInstanceContext'} -> `Word32'#}
{#fun unsafe ModuleInstanceWASIGetNativeHandler as ^ {`ModuleInstanceContext',`Word32',`Word64'} -> `Word32'#} -- at Word64 substituting uint64_t *
-- {#fun unsafe ModuleInstanceInitWasmEdgeProcess as ^ {`Char* Const',`Word32',`Bool'} -> `()'#} --
{#fun unsafe ModuleInstanceGetModuleNameOut as moduleInstanceGetModuleName {+,`ModuleInstanceContext'} -> `WasmString'#} --
{#fun unsafe ModuleInstanceGetHostData as ^ {`ModuleInstanceContext'} -> `()'#} --
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
-- {#fun unsafe FunctionInstanceCreate as ^ {`FunctionTypeContext',`HostFunc_t',`void *',`Word64'} -> `FunctionTypeContext'#}  --some random typedef
-- {#fun unsafe FunctionInstanceCreateBinding as ^ {`FunctionTypeContext',`WrapFunc_t',`void *',`void *',`Word64'} -> `FunctionInstanceContext'#} -- some random typedef
{#fun unsafe FunctionInstanceGetFunctionType as ^ {`FunctionInstanceContext'} -> `FunctionTypeContext'#}

-- Table Instance
{#fun unsafe TableInstanceCreate as ^ {`TableTypeContext'} -> `TableInstanceContext'#}
{#fun unsafe TableInstanceGetTableType as ^ {`TableInstanceContext'} -> `TableTypeContext'#}
{#fun unsafe TableInstanceGetDataOut as tableInstanceGetData  {+,`TableInstanceContext',`WasmVal',`Word32'} -> `WasmResult'#} -- WasmValue, WasmResult
{#fun unsafe TableInstanceSetDataOut as tableInstanceSetData  {+,`TableInstanceContext',`WasmVal',`Word32'} -> `WasmResult'#} -- WasmValue, WasmResult
{#fun unsafe TableInstanceGetSize as ^ {`TableInstanceContext'} -> `Word32'#} 
{#fun unsafe TableInstanceGrowOut as tableInstanceGrow {+,`TableInstanceContext',`Word32'} -> `WasmResult'#}

-- Memory Instance
{#fun unsafe MemoryInstanceCreate as ^ {`MemoryTypeContext'} -> `MemoryInstanceContext'#} 
{#fun unsafe MemoryInstanceGetMemoryType as ^ {`MemoryInstanceContext'} -> `MemoryTypeContext'#} 
{#fun unsafe MemoryInstanceGetDataOut as memoryInstanceGetData {+,`MemoryInstanceContext',`Word8',`Word32',`Word32'} -> `WasmResult'#} 
-- {#fun unsafe MemoryInstanceSetDataOut as memoryInstanceSetData {+,`MemoryInstanceContext',`Vector Word8'&} -> `WasmResult'#} -- use wrapper with offset before data
-- {#fun unsafe MemoryInstanceGetPointer as ^ {`MemoryInstanceContext',`Word32',`Word32'} -> `Vector Word8'#} -- Haskell type: Ptr Word8 C type      : (IO (C2HSImp.Ptr C2HSImp.CUChar))
-- {#fun unsafe MemoryInstanceGetPointerConst as ^ {`MemoryInstanceContext',`Word32',`Word32'} -> `Word8'#} --Haskell type: Ptr Word8 C type      : (IO (C2HSImp.Ptr C2HSImp.CUChar))
{#fun unsafe MemoryInstanceGetPageSize as ^ {`MemoryInstanceContext'} -> `Word32'#} 
{#fun unsafe MemoryInstanceGrowPageOut as memoryInstanceGrowPage {+,`MemoryInstanceContext',`Word32'} -> `WasmResult'#} 

-- Global Instance
{#fun unsafe GlobalInstanceCreateOut as globalInstanceCreate {`GlobalTypeContext',`WasmVal'} -> `GlobalInstanceContext'#}
{#fun unsafe GlobalInstanceGetGlobalType as ^ {`GlobalInstanceContext'} -> `GlobalTypeContext'#} 
-- {#fun unsafe GlobalInstanceGetValueOut as globalInstanceGetValue  {`GlobalInstanceContext'} -> `WasmVal'#} --How to return wasmvalue 
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
{#fun unsafe AsyncGetOut as asyncGet {+,`Async',`WasmVal',`Word32'} -> `WasmResult'#} -- wasmresult

-- VM
{#fun unsafe VMCreate as ^ {`ConfigureContext',`StoreContext'} -> `VMContext'#}
{#fun unsafe VMRegisterModuleFromFileOut as vMRegisterModuleFromFile {+,`VMContext',%`WasmString',`String'} -> `WasmResult'#}
{#fun unsafe VMRunWasmFromFileOut as vMRunWasmFromFile {+,`VMContext',`String',%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe VMRunWasmFromASTModuleOut as vMRunWasmFromASTModule {+,`VMContext',`ASTModuleContext',%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe VMAsyncRunWasmFromFileOut as vMAsyncRunWasmFromFile {`VMContext',`String',%`WasmString',`WasmVal',`Word32'} -> `Async'#} --wasmresult
{#fun unsafe VMAsyncRunWasmFromASTModuleOut as vMAsyncRunWasmFromASTModule  {`VMContext',`ASTModuleContext',%`WasmString',`WasmVal',`Word32'} -> `Async'#} --wasmresult
{-
{#fun unsafe VMRegisterModuleFromBuffer as ^ {`VMContext',`WasmString',`Word8',`Word32'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMRunWasmFromBuffer as ^ {`VMContext',`Word8',`Word32',`WasmString',`WasmValue',`Word32',`WasmValue',`Word32'} -> `WasmResult'#} --wasmresult + word8
{#fun unsafe VMAsyncRunWasmFromBuffer as ^ {`VMContext',`Word8',`WasmString',`WasmValue',`Word32'} -> `Async'#} --wasmresult
{#fun unsafe VMLoadWasmFromBuffer as ^ {`VMContext',`Word8',`Word32'} -> `WasmResult'#} --wasmresult
-}
{#fun unsafe VMRegisterModuleFromASTModuleOut as vMRegisterModuleFromASTModule {+,`VMContext',%`WasmString',`ASTModuleContext'} -> `WasmResult'#}
{#fun unsafe VMRegisterModuleFromImportOut as vMRegisterModuleFromImport {+,`VMContext',`ModuleInstanceContext'} -> `WasmResult'#}
{#fun unsafe VMLoadWasmFromFileOut as vMLoadWasmFromFile {+,`VMContext',`String'} -> `WasmResult'#}
{#fun unsafe VMLoadWasmFromASTModuleOut as vMLoadWasmFromASTModule {+,`VMContext',`ASTModuleContext'} -> `WasmResult'#}
{#fun unsafe VMValidateOut as vMValidate  {+,`VMContext'} -> `WasmResult'#}
{#fun unsafe VMInstantiateOut as vMInstantiate  {+,`VMContext'} -> `WasmResult'#}
{#fun unsafe VMExecuteOut as vMExecuteOut {+,`VMContext',%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe VMExecuteRegisteredOut as vMExecuteRegistered {+,`VMContext',%`WasmString',%`WasmString',`WasmVal',`Word32',`WasmVal',`Word32'} -> `WasmResult'#}
{#fun unsafe VMAsyncExecuteOut as vMAsyncExecute {`VMContext',%`WasmString',`WasmVal',`Word32'} -> `Async'#}
{#fun unsafe VMAsyncExecuteRegisteredOut as vMAsyncExecuteRegistered {`VMContext',%`WasmString',%`WasmString',`WasmVal',`Word32'} -> `Async'#} --wasmresult
{#fun unsafe VMGetFunctionType as ^ {`VMContext',%`WasmString'} -> `FunctionTypeContext'#}
{#fun unsafe VMGetFunctionTypeRegistered as ^ {`VMContext',%`WasmString',%`WasmString'} -> `FunctionTypeContext'#}
{#fun unsafe VMCleanup as ^ {`VMContext'} -> `()'#} 
{#fun unsafe VMGetFunctionListLength as ^ {`VMContext'} -> `Word32'#} 
-- {#fun unsafe VMGetFunctionList as ^ {`VMContext',`WasmString',`FunctionTypeContext',`Word32'} -> `Word32'#} --Double pointer, Expected FunctionTypeContext Actual Ptr FunctionTypeContext
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
