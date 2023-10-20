module WasmEdge.Memory
  ( MemoryTypeContext
  , MemoryInstanceContext 
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
