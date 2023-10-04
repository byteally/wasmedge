module WasmEdge.Loader
  (LoaderContext,
  loaderCreate,
  loaderParseFromFile,
  loaderParseFromBuffer
  ) where

import WasmEdge.Internal.FFI.Bindings (
    LoaderContext,
  loaderCreate,
  loaderParseFromFile,
  loaderParseFromBuffer)