{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module WasmEdge.Internal.FFI.ValueTypes
  ( i32Value
  , mkString
  , deleteString
  , wasmStringEq
  , wasmStringLength
  , toText
  , mkStringFromBytes
  , stringCopy
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
-- import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Word
-- import Foreign
import Foreign.C
-- import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
-- import GHC.Ptr
import System.IO.Unsafe
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

#include "wasmedge/wasmedge.h"

{#fun pure unsafe WasmEdge_ValueGenI32 as i32Value
   { `Int32'
   } -> `()'
#}

#c
void StringCreateByCStringOut(WasmEdge_String* strOut, const char *Str)
{
  WasmEdge_String str = WasmEdge_StringCreateByCString(Str);
  strOut->Length = str.Length;
  strOut->Buf = str.Buf;
}
void StringCreateByBufferOut(WasmEdge_String* strOut, const char *Str, const uint32_t Len)
{
  WasmEdge_String str = WasmEdge_StringCreateByBuffer(Str, Len);
  strOut->Length = str.Length;
  strOut->Buf = str.Buf;
}

WasmEdge_String StringWrapOut(WasmEdge_String* strOut, const char *Buf, const uint32_t Len)
{
  WasmEdge_String str = WasmEdge_StringWrap(Buf, Len);
  strOut->Length = str.Length;
  strOut->Buf = str.Buf;
}
void C_Result_Success(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Success.Code;}
void C_Result_Terminate(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Terminate.Code;}
void C_Result_Fail(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Fail.Code;}
#endc

-- {#pointer *WasmEdge_Value as WasmValue foreign newtype #}
{#pointer *WasmEdge_String as WasmString foreign finalizer WasmEdge_StringDelete as deleteString newtype #}
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


{#fun pure unsafe StringCreateByCStringOut as mkString {+, `String'} -> `WasmString' #}
{#fun pure unsafe StringCreateByBufferOut as mkStringFromBytes {+, useAsCStringLenBS*`ByteString'& packCStringLenBS*} -> `WasmString' #}
-- {#fun pure unsafe StringWrapOut as stringWrap {+, `String'} -> `WasmString' #}
{#fun pure unsafe WasmEdge_StringIsEqual as wasmStringEq {%`WasmString', %`WasmString'} -> `Bool' #}
{#fun pure unsafe WasmEdge_StringCopy as stringCopy {%`WasmString', memBuffIn*`MemBuff'&} -> `Word32' #}
{#fun pure unsafe C_Result_Success as mkResultSuccess {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Terminate as mkResultTerminate {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Fail as mkResultFail {+} -> `WasmResult' #}

useAsCStringLenBS :: ByteString -> ((CString, CUInt) -> IO a) -> IO a
useAsCStringLenBS bs f = BS.useAsCStringLen bs (\strLen -> f (fromIntegral <$> strLen))

packCStringLenBS :: CString -> CUInt -> IO ByteString
packCStringLenBS cstr len = BS.packCStringLen (cstr, fromIntegral len)

data MemBuff = MemBuff {memBuffLen :: Int, memBuff :: ForeignPtr CChar}

allocMemBuff :: Int -> IO MemBuff
allocMemBuff sz = MemBuff sz <$> mallocForeignPtrBytes sz

memBuffIn :: MemBuff -> ((Ptr CChar, CUInt) -> IO a) -> IO a
memBuffIn mem f = withForeignPtr (memBuff mem) $ \p -> (f (p, fromIntegral $ memBuffLen mem)) 

instance Eq WasmString where
  (==) = wasmStringEq

instance IsString WasmString where
  fromString = mkString

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

{#pointer *WasmEdge_ConfigureContext as ConfigureContext foreign finalizer WasmEdge_ConfigureDelete as deleteConfigureContext newtype #}
{#pointer *WasmEdge_StatisticsContext as StatisticsContext foreign finalizer WasmEdge_StatisticsDelete as deleteStatisticsContext newtype #}
{#pointer *WasmEdge_ASTModuleContext as ASTModuleContext foreign finalizer WasmEdge_ASTModuleDelete as deleteASTModule newtype #}
{#pointer *WasmEdge_FunctionTypeContext as FunctionTypeContext foreign finalizer WasmEdge_FunctionTypeDelete as deleteFunctionTypeContext newtype #}
{#pointer *WasmEdge_MemoryTypeContext as MemoryTypeContext foreign finalizer WasmEdge_MemoryTypeDelete as deleteMemoryTypeContext newtype #}
{#pointer *WasmEdge_TableTypeContext as TableTypeContext foreign finalizer WasmEdge_TableTypeDelete as deleteTableTypeContext newtype #}
{#pointer *WasmEdge_GlobalTypeContext as GlobalTypeContext foreign finalizer WasmEdge_GlobalTypeDelete as deleteGlobalTypeContext newtype #}
{#pointer *WasmEdge_ImportTypeContext as ImportTypeContext foreign newtype #}
{#pointer *WasmEdge_ExportTypeContext as ExportTypeContext foreign newtype #}
{#pointer *WasmEdge_CompilerContext as CompilerContext foreign finalizer WasmEdge_CompilerDelete as deleteCompilerContext newtype #}
{#pointer *WasmEdge_LoaderContext as LoaderContext foreign finalizer WasmEdge_LoaderDelete as deleteLoaderContext newtype #}
{#pointer *WasmEdge_ValidatorContext as ValidatorContext foreign finalizer WasmEdge_ValidatorDelete as deleteValidatorContext newtype #}
{#pointer *WasmEdge_ExecutorContext as ExecutorContext foreign finalizer WasmEdge_ExecutorDelete as deleteExecutorContext newtype #}
{#pointer *WasmEdge_StoreContext as StoreContext foreign finalizer WasmEdge_StoreDelete as deleteStore newtype #}
{#pointer *WasmEdge_ModuleInstanceContext as ModuleInstanceContext foreign finalizer WasmEdge_ModuleInstanceDelete as deleteModuleInstanceContext newtype #}
{#pointer *WasmEdge_FunctionInstanceContext as FunctionInstanceContext foreign finalizer WasmEdge_FunctionInstanceDelete as deleteFunctionInstanceContext newtype #}
{#pointer *WasmEdge_TableInstanceContext as TableInstanceContext foreign finalizer WasmEdge_TableInstanceDelete as deleteTableInstanceContext newtype #}
{#pointer *WasmEdge_MemoryInstanceContext as MemoryInstanceContext foreign finalizer WasmEdge_MemoryInstanceDelete as deleteMemoryInstanceContext newtype #}
{#pointer *WasmEdge_GlobalInstanceContext as GlobalInstanceContext foreign finalizer WasmEdge_GlobalInstanceDelete as deleteGlobalInstanceContext newtype #}
{#pointer *WasmEdge_CallingFrameContext as CallingFrameContext foreign newtype #}
{#pointer *WasmEdge_Async as Async foreign finalizer WasmEdge_AsyncDelete as deleteAsync newtype #}
{#pointer *WasmEdge_VMContext as VMContext foreign finalizer WasmEdge_VMDelete as deleteVMContext newtype #}
{#pointer *WasmEdge_PluginContext as PluginContext foreign newtype #}

{#enum WasmEdge_ProgramOptionType as ProgramOptionType {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WasmEdge logging functions
{#fun pure unsafe WasmEdge_LogSetErrorLevel as setLogErrorLevel {} -> `()'#}
{#fun pure unsafe WasmEdge_LogSetDebugLevel as setLogDebugLevel {} -> `()'#}

-- WASM Proposal C enumeration.
{#enum WasmEdge_Proposal as Proposal {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Host Module Registration C enumeration.
{#enum WasmEdge_HostRegistration as HostRegistration {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- AOT compiler optimization level C enumeration.
{#enum WasmEdge_CompilerOptimizationLevel as CompilerOptimizationLevel {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- AOT compiler output binary format C enumeration.
{#enum WasmEdge_CompilerOutputFormat as CompilerOutputFormat {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Error category C enumeration.
{#enum WasmEdge_ErrCategory as ErrCategory {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Error code C enumeration.
{#enum WasmEdge_ErrCode as ErrCode {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Value type C enumeration.
{#enum WasmEdge_ValType as ValType {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Number type C enumeration.
{#enum WasmEdge_NumType as NumType {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Reference type C enumeration.
{#enum WasmEdge_RefType as RefType {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Mutability C enumeration.
{#enum WasmEdge_Mutability as Mutability {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM External type C enumeration.
{#enum WasmEdge_ExternalType as ExternalType {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}
