module WasmEdge.Module
  ( module WasmEdge.Internal.Module
  , astModuleListImports
  , astModuleListExports
  , Vector
  ) where

import WasmEdge.Internal.Module
  hiding ( astModuleListImports
         , astModuleListExports
         )
import qualified WasmEdge.Internal.FFI.Bindings as Int
import Data.Vector.Storable (Vector)

astModuleListImports
  :: ASTModuleContext
  -> IO (Vector Int.ImportTypeContext)
astModuleListImports ast = Int.astModuleListImports ast =<< astModuleListImportsLength ast

astModuleListExports
  :: ASTModuleContext
  -> IO (Vector Int.ExportTypeContext)
astModuleListExports ast = Int.astModuleListExports ast =<< astModuleListExportsLength ast
  
