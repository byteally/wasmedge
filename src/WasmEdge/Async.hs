module WasmEdge.Async
  ( module WasmEdge.Internal.Async 
 , asyncGet
 ,Vector
  ) where

import WasmEdge.Internal.Async
 hiding (asyncGet)

import qualified WasmEdge.Internal.FFI.Bindings as Int
import Data.Vector as V

asyncGet :: 
 Async 
 -> IO (Int.WasmResult,V.Vector Int.WasmVal)
asyncGet async = Int.asyncGet async =<< asyncGetReturnsLength async
