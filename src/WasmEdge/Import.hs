module WasmEdge.Import
  (ImportTypeContext,
  importTypeGetModuleName,
  importTypeGetExternalName,
  importTypeGetFunctionType,
  importTypeGetTableType,
  importTypeGetMemoryType,
  importTypeGetGlobalType
  ) where

import WasmEdge.Internal.FFI.Bindings (
   ImportTypeContext,
  importTypeGetModuleName,
  importTypeGetExternalName,
  importTypeGetFunctionType,
  importTypeGetTableType,
  importTypeGetMemoryType,
  importTypeGetGlobalType)