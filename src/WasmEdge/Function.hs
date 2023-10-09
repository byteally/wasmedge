module WasmEdge.Function
  ( module WasmEdge.Internal.Function
  , functionTypeGetParameters
  , functionTypeGetReturns
  , Vector
  ) where

import WasmEdge.Internal.Function
 hiding (functionTypeGetParameters,functionTypeGetReturns)

import qualified WasmEdge.Internal.FFI.Bindings as Int
import Data.Vector.Storable (Vector)

functionTypeGetParameters ::
 FunctionTypeContext
 -> IO (Vector Int.ValType)
functionTypeGetParameters fcxt = Int.functionTypeGetParameters fcxt =<< functionTypeGetParametersLength fcxt

functionTypeGetReturns ::
 FunctionTypeContext
 -> IO (Vector Int.ValType)
functionTypeGetReturns fcxt = Int.functionTypeGetReturns fcxt =<< functionTypeGetReturnsLength fcxt
