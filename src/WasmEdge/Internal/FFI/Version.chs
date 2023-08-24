module WasmEdge.Internal.FFI.Version
  ( versionGet
  , versionGetMajor
  , versionGetMinor
  , versionGetPatch
  ) where

import Data.Text (Text)
import qualified Data.Text.Foreign as T
import Foreign
import Foreign.C

#include "wasmedge/wasmedge.h"

{#context prefix = "WasmEdge"#}

{#fun pure unsafe VersionGet as ^ {} -> `Text' fromCStrToText*#}

{#fun pure unsafe VersionGetMajor as ^ {} -> `Word' fromIntegral#}

{#fun pure unsafe VersionGetMinor as ^ {} -> `Word' fromIntegral#}

{#fun pure unsafe VersionGetPatch as ^ {} -> `Word' fromIntegral#}
    

fromCStrToText :: CString -> IO Text
fromCStrToText cs = T.fromPtr0 $ castPtr cs
