module WasmEdge.Store
  ( module WasmEdge.Internal.Store 
  , storeListModule
  ) where

import WasmEdge.Internal.Store 
 hiding (storeListModule)

import qualified WasmEdge.Internal.FFI.Bindings as Int
import Data.Vector as V

storeListModule ::
 StoreContext
 -> IO (V.Vector Int.WasmString)
storeListModule store = Int.storeListModule store =<< storeListModuleLength store
