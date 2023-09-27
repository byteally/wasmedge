module WasmEdge.Compiler
  (CompilerContext,
  compilerCreate,
  compilerCompile,
  compilerCompileFromBuffer
  ) where

import WasmEdge.Internal.FFI.Bindings (
   CompilerContext,
  compilerCreate,
  compilerCompile,
  compilerCompileFromBuffer)