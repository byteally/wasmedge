module WasmEdge.Plugin
  ( module WasmEdge.Internal.Plugin
  ,pluginListPlugins
  ,pluginListModule
  ) where

import WasmEdge.Internal.Plugin
 hiding (pluginListPlugins,pluginListModule)

import qualified WasmEdge.Internal.FFI.Bindings as Int
import Data.Vector as V

pluginListPlugins 
 :: IO (V.Vector Int.WasmString)
pluginListPlugins = Int.pluginListPlugins =<< pluginListPluginsLength

pluginListModule ::
 PluginContext
 -> IO (V.Vector Int.WasmString)
pluginListModule pcxt = Int.pluginListModule pcxt =<< pluginListModuleLength pcxt
