module WasmEdge.Calling
  ( CallingFrameContext
  , callingFrameGetExecutor
  , callingFrameGetModuleInstance
  , callingFrameGetMemoryInstance
  ) where

import WasmEdge.Internal.FFI.Bindings
    ( callingFrameGetExecutor,
      callingFrameGetMemoryInstance,
      callingFrameGetModuleInstance,
      CallingFrameContext )
