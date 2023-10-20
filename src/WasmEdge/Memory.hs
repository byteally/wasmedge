module WasmEdge.Memory
  ( MemoryTypeContext
  , memoryInstanceContext 
  , memoryInstanceCreate 
  , memoryInstanceGetMemoryType 
  , memoryInstanceGetData
  , memoryInstanceSetData
  , memoryInstanceGetPointer
  , memoryInstanceGetPointerConst
  , memoryInstanceGetPageSize
  , memoryInstanceGrowPage
  , memoryTypeCreate
  , memoryTypeGetLimit
  ) where

import WasmEdge.Internal.FFI.Bindings
