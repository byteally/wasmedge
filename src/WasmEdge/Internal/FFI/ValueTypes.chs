{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module WasmEdge.Internal.FFI.ValueTypes
  ( i32Value
  , mkString
  , deleteString
  , wasmStringEq
  , wasmStringLength
  , toText
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
-- import Foreign.ForeignPtr
-- import GHC.Ptr
import System.IO.Unsafe

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
#endc

{#pointer *WasmEdge_String as WasmString foreign finalizer WasmEdge_StringDelete as deleteString newtype #}

{#fun pure unsafe StringCreateByCStringOut as mkString {+, `String'} -> `WasmString' #}

{#fun pure unsafe WasmEdge_StringIsEqual as wasmStringEq {%`WasmString', %`WasmString'} -> `Bool' #}

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

{#pointer *WasmEdge_ModuleDescriptor as ModuleDescriptor foreign newtype #}
{#pointer *WasmEdge_PluginVersionData as PluginVersionData foreign newtype #}
{#pointer *WasmEdge_PluginDescriptor as PluginDescriptor foreign newtype #}

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
