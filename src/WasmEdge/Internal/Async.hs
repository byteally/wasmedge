module WasmEdge.Internal.Async
  ( Async
  , asyncWait
  , asyncWaitFor
  , asyncCancel
  , asyncGetReturnsLength
  , asyncGet
  ) where

import WasmEdge.Internal.FFI.Bindings
    ( asyncCancel,
      asyncGet,
      asyncGetReturnsLength,
      asyncWait,
      asyncWaitFor,
      Async )
