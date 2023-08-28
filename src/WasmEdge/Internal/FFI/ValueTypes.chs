{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module WasmEdge.Internal.FFI.ValueTypes
  ( i32Value
  , deleteString
  , wasmStringEq
  , wasmStringLength
  , toText
  , mkStringFromBytes
  , stringCopy
  , configureAddHostRegistration
  , logSetErrorLevel
  , logSetDebugLevel
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

-- import Data.Vector.Storable.Mutable (IOVector)
-- import qualified Data.Vector.Storable.Mutable as VSM

#include "wasmedge/wasmedge.h"
#include <stdio.h>

{#context prefix = "WasmEdge"#}

{#fun pure unsafe WasmEdge_ValueGenI32 as i32Value
   { `Int32'
   } -> `()'
#}

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
void ExecutorInvokeOut(WasmEdge_Result *resOut, WasmEdge_ExecutorContext *Cxt,
                        const WasmEdge_FunctionInstanceContext *FuncCxt,
                        const WasmEdge_Value *Params, const uint32_t ParamLen,
                        WasmEdge_Value *Returns, const uint32_t ReturnLen){ *resOut = WasmEdge_ExecutorInvoke (Cxt,FuncCxt,Params,ParamLen,Returns,ReturnLen); }
void ModuleInstanceGetModuleNameOut(WasmEdge_String* strOut,WasmEdge_ModuleInstanceContext* Ctx){ *strOut = WasmEdge_ModuleInstanceGetModuleName(Ctx); }
void TableInstanceGrowOut(WasmEdge_Result* resOut,WasmEdge_TableInstanceContext* Ctx,const uint32_t Size){ *resOut = WasmEdge_TableInstanceGrow(Ctx,Size); }
void MemoryInstanceGetDataOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext* Ctx,uint8_t *Data, const uint32_t Offset,const uint32_t Length){ 
*resOut = WasmEdge_MemoryInstanceGetData(Ctx,Data,Offset,Length); }
void MemoryInstanceSetDataOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext* Ctx,uint8_t *Data, const uint32_t Offset,const uint32_t Length){ 
*resOut = WasmEdge_MemoryInstanceSetData(Ctx,Data,Offset,Length); }
void MemoryInstanceGrowPageOut(WasmEdge_Result* resOut,WasmEdge_MemoryInstanceContext *Cxt,const uint32_t Page){ *resOut = WasmEdge_MemoryInstanceGrowPage(Cxt,Page); }
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

-- {#pointer *WasmEdge_Value as WasmValue foreign newtype #}
{#pointer *WasmEdge_String as WasmString foreign finalizer StringDeleteByPtr as deleteString newtype #}
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


{#fun unsafe StringCreateByBufferOut as mkStringFromBytesIO {+, useAsCStringLenBS*`ByteString'& } -> `WasmString' #}
{#fun pure unsafe StringWrapOut as stringWrap {+, useAsCStringLenBS*`ByteString'&} -> `WasmString' #}
{#fun pure unsafe WasmEdge_StringIsEqual as wasmStringEq {%`WasmString', %`WasmString'} -> `Bool' #}
{#fun pure unsafe WasmEdge_StringCopy as _stringCopy {%`WasmString', memBuffIn*`MemBuff'&} -> `Word32' #}
{#fun pure unsafe C_Result_Success as mkResultSuccess {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Terminate as mkResultTerminate {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Fail as mkResultFail {+} -> `WasmResult' #}

mkStringFromBytes :: ByteString -> WasmString
mkStringFromBytes bs = unsafePerformIO $ do
  ws@(WasmString fp) <- mkStringFromBytesIO bs
  addForeignPtrFinalizer deleteString fp
  pure ws

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
-- {#fun unsafe ExecutorInvokeOut as executorInvoke {+,`ExecutorContext',`FunctionInstanceContext',`WasmValue',`Word32',`WasmValue',`Word32'} -> `WasmResult'#}
-- {#fun unsafe ExecutorAsyncInvoke as ^ {`ExecutorContext',`FunctionInstanceContext',%`WasmValue',`Word32'} -> `Async'#} -- WasmValue and Word32 

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
-- {#fun unsafe TableInstanceGetData as ^ {`TableInstanceContext',`WasmValue',`Word32'} -> `WasmResult'#} -- WasmValue, WasmResult
-- {#fun unsafe TableInstanceSetData as ^ {`TableInstanceContext',`WasmValue',`Word32'} -> `WasmResult'#} -- WasmValue, WasmResult
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
-- {#fun unsafe GlobalInstanceCreate as ^ {`GlobalTypeContext',`WasmValue'} -> `GlobalInstanceContext'#} --WasmValue 
{#fun unsafe GlobalInstanceGetGlobalType as ^ {`GlobalInstanceContext'} -> `GlobalTypeContext'#} 
-- {#fun unsafe GlobalInstanceGetValue as ^ {`GlobalInstanceContext'} -> `WasmValue'#} --wasmvalue 
-- {#fun unsafe GlobalInstanceSetValue as ^ {`GlobalInstanceContext',`WasmValue'} -> `()'#} --wasmvalue 

-- Calling Frame
{#fun unsafe CallingFrameGetExecutor as ^ {`CallingFrameContext'} -> `ExecutorContext'#}
{#fun unsafe CallingFrameGetModuleInstance as ^ {`CallingFrameContext'} -> `ModuleInstanceContext'#}
{#fun unsafe CallingFrameGetMemoryInstance as ^ {`CallingFrameContext',`Word32'} -> `MemoryInstanceContext'#}

-- Async
{#fun unsafe AsyncWait as ^ {`Async'} -> `()'#}
{#fun unsafe AsyncWaitFor as ^ {`Async',`Word64'} -> `Bool'#}
{#fun unsafe AsyncCancel as ^ {`Async'} -> `()'#}
{#fun unsafe AsyncGetReturnsLength as ^ {`Async'} -> `Word32'#}
-- {#fun unsafe AsyncGetOut as asyncGet {+,`Async',`WasmValue',`Word32'} -> `WasmResult'#} -- wasmresult

-- VM
{#fun unsafe VMCreate as ^ {`ConfigureContext',`StoreContext'} -> `VMContext'#}
{#fun unsafe VMRegisterModuleFromFileOut as vMRegisterModuleFromFile {+,`VMContext',%`WasmString',`String'} -> `WasmResult'#} --wasmresult

{-
{#fun unsafe VMRegisterModuleFromBuffer as ^ {`VMContext',`WasmString',`Word8',`Word32'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMRunWasmFromFile as ^ {`VMContext',`String',`WasmString',`WasmValue',`Word32',`WasmValue',`Word32'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMRunWasmFromBuffer as ^ {`VMContext',`Word8',`Word32',`WasmString',`WasmValue',`Word32',`WasmValue',`Word32'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMRunWasmFromASTModule as ^ {`VMContext',`ASTModuleContext',`WasmString',`WasmValue',`Word32',`WasmValue',`Word32'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMAsyncRunWasmFromFile as ^ {`VMContext',`String',`WasmString',`WasmValue',`Word32'} -> `Async'#} --wasmresult
{#fun unsafe VMAsyncRunWasmFromBuffer as ^ {`VMContext',`Word8',`WasmString',`WasmValue',`Word32'} -> `Async'#} --wasmresult
{#fun unsafe VMAsyncRunWasmFromASTModule as ^ {`VMContext',`ASTModuleContext',`WasmString',`WasmValue',`Word32'} -> `Async'#} --wasmresult
{#fun unsafe VMLoadWasmFromBuffer as ^ {`VMContext',`Word8',`Word32'} -> `WasmResult'#} --wasmresult
-}
{#fun unsafe VMRegisterModuleFromASTModuleOut as vMRegisterModuleFromASTModule {+,`VMContext',%`WasmString',`ASTModuleContext'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMRegisterModuleFromImportOut as vMRegisterModuleFromImport {+,`VMContext',`ModuleInstanceContext'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMLoadWasmFromFileOut as vMLoadWasmFromFile {+,`VMContext',`String'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMLoadWasmFromASTModuleOut as vMLoadWasmFromASTModule {+,`VMContext',`ASTModuleContext'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMValidateOut as vMValidate  {+,`VMContext'} -> `WasmResult'#}
{#fun unsafe VMInstantiateOut as vMInstantiate  {+,`VMContext'} -> `WasmResult'#}
{-
{#fun unsafe VMExecute as ^ {`VMContext',`WasmString',`WasmValue',`Word32',`WasmValue',`Word32'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMExecuteRegistered as ^ {`VMContext',`WasmString',`WasmString',`WasmValue',`Word32',`WasmValue',`Word32'} -> `WasmResult'#} --wasmresult
{#fun unsafe VMAsyncExecute as ^ {`VMContext',`WasmString',`WasmValue',`Word32'} -> `Async'#} --wasmresult
{#fun unsafe VMAsyncExecuteRegistered as ^ {`VMContext',%`WasmString',`WasmValue',`Word32'} -> `Async'#} --wasmresult
-}
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
