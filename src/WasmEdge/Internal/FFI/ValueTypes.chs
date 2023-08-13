module WasmEdge.Internal.FFI.ValueTypes
  ( i32Value
  , ValType (..)
  , WasmString
  , mkString
  , deleteString
  , wasmStringEq
  , wasmStringLength
  ) where

import Data.Int
import Data.Word
-- import Foreign
-- import Foreign.C
-- import Foreign.ForeignPtr
-- import GHC.Ptr
-- import System.IO.Unsafe

#include "wasmedge/wasmedge.h"

{#enum WasmEdge_ValType as ValType {}
  with prefix = "WasmEdge_ValType"
  deriving (Show, Eq) #}

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

