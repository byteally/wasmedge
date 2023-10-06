module WasmEdge.Function
  ( FunctionTypeContext
  , functionTypeCreate
  , functionTypeGetParametersLength
  , functionTypeGetParameters
  , functionTypeGetReturnsLength
  , functionTypeGetReturns
  , Vector
  ) where

import WasmEdge.Internal.Module
	hiding (functionTypeGetParameters
			,functionTypeGetReturns
	)

import qualified WasmEdge.Internal.FFI.Bindings as Int
import Data.Vector.Storable (Vector)

import WasmEdge.Internal.FFI.Bindings
    ( FunctionTypeContext
  , functionTypeCreate
  , functionTypeGetParametersLength
  , functionTypeGetReturnsLength)

functionTypeGetParameters ::
 FunctionTypeContext
 -> IO (Vector ValType)
functionTypeGetParameters fcxt = Int.functionTypeGetParameters fcxt =<< functionTypeGetParametersLength fcxt

functionTypeGetReturns ::
 FunctionTypeContext
 -> IO (Vector ValType)
functionTypeGetReturns fcxt = Int.functionTypeGetReturns fcxt =<< functionTypeGetReturnsLength fcxt
