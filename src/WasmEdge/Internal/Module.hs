module WasmEdge.Internal.Module
  ( ModuleDescriptor
  , ASTModuleContext
  , ModuleInstanceContext
  , astModuleListImportsLength
  , astModuleListImports
  , astModuleListExportsLength
  , astModuleListExports
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
  ) where

import WasmEdge.Internal.FFI.Bindings
