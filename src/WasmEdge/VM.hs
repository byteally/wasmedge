module WasmEdge.VM
  ( module WasmEdge.Internal.VM
  , vmListRegisteredModule
  , vmGetFunctionList
  ) where

import WasmEdge.Internal.VM 
 hiding (vmListRegisteredModule,vmGetFunctionList)

import qualified WasmEdge.Internal.FFI.Bindings as Int
import Data.Vector as V

vmListRegisteredModule 
 :: VMContext 
 -> IO (V.Vector Int.WasmString)
vmListRegisteredModule vm = Int.vmListRegisteredModule vm =<< vmListRegisteredModuleLength vm

vmGetFunctionList
 :: VMContext
 -> IO (V.Vector Int.WasmString,V.Vector Int.FunctionTypeContext)
vmGetFunctionList vm = Int.vmGetFunctionList vm =<< vmGetFunctionListLength vm
