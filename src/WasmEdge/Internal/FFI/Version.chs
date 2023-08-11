module WasmEdge.Internal.FFI.Version
  ( getVersion
  , getMajorVersion
  , getMinorVersion
  ) where

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Foreign
import Foreign.C
import System.IO.Unsafe

#include "wasmedge/wasmedge.h"

-- versionGet :: Text
-- versionGet = unsafePerformIO $ do
--   cstr <- {#call unsafe WasmEdge_VersionGet as versionGet1 #}
--   T.fromPtr0 (castPtr cstr)

{#fun pure unsafe WasmEdge_VersionGet as getVersion {} -> `Text' fromCStrToText#}

{#fun pure unsafe WasmEdge_VersionGetMajor as getMajorVersion {} -> `Word' fromIntegral#}

{#fun pure unsafe WasmEdge_VersionGetMinor as getMinorVersion {} -> `Word' fromIntegral#}
    

-- fromCStrToTextIO :: CString -> IO Text
-- fromCStrToTextIO = T.fromPtr0 . castPtr

fromCStrToText :: CString -> Text
fromCStrToText cs = unsafePerformIO $ T.fromPtr0 $ castPtr cs
