module WasmEdge.VM
  ( VMContext
  , vmCreate
  , vmRegisterModuleFromFile
  , vmRunWasmFromFile
  , vmRunWasmFromBuffer
  , vmRunWasmFromASTModule
  , vmAsyncRunWasmFromFile
  , vmAsyncRunWasmFromASTModule
  , vmAsyncRunWasmFromBuffer
  , vmRegisterModuleFromBuffer
  , vmRegisterModuleFromASTModule
  , vmRegisterModuleFromImport
  , vmLoadWasmFromFile
  , vmLoadWasmFromBuffer
  , vmLoadWasmFromASTModule
  , vmValidate
  , vmInstantiate
  , vmExecute
  , vmExecuteRegistered
  , vmAsyncExecute
  , vmAsyncExecuteRegistered
  , vmGetFunctionType
  , vmGetFunctionTypeRegistered
  , vmCleanup
  , vmGetFunctionListLength
  , vmGetFunctionList
  , vmGetImportModuleContext
  , vmGetActiveModule
  , vmGetRegisteredModule
  , vmListRegisteredModuleLength
  , vmListRegisteredModule
  , vmGetStoreContext
  , vmGetLoaderContext
  , vmGetValidatorContext
  , vmGetExecutorContext
  , vmGetStatisticsContext
  ) where

import WasmEdge.Internal.FFI.Bindings
    ( vmAsyncExecute,
      vmAsyncExecuteRegistered,
      vmAsyncRunWasmFromASTModule,
      vmAsyncRunWasmFromBuffer,
      vmAsyncRunWasmFromFile,
      vmCleanup,
      vmCreate,
      vmExecute,
      vmExecuteRegistered,
      vmGetActiveModule,
      vmGetExecutorContext,
      vmGetFunctionList,
      vmGetFunctionListLength,
      vmGetFunctionType,
      vmGetFunctionTypeRegistered,
      vmGetImportModuleContext,
      vmGetLoaderContext,
      vmGetRegisteredModule,
      vmGetStatisticsContext,
      vmGetStoreContext,
      vmGetValidatorContext,
      vmInstantiate,
      vmListRegisteredModule,
      vmListRegisteredModuleLength,
      vmLoadWasmFromASTModule,
      vmLoadWasmFromBuffer,
      vmLoadWasmFromFile,
      vmRegisterModuleFromASTModule,
      vmRegisterModuleFromBuffer,
      vmRegisterModuleFromFile,
      vmRegisterModuleFromImport,
      vmRunWasmFromASTModule,
      vmRunWasmFromBuffer,
      vmRunWasmFromFile,
      vmValidate,
      VMContext )
