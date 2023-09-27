module WasmEdge.VM
  ( VMContext
  , vMCreate
  , vMRegisterModuleFromFile
  , vMRunWasmFromFile
  , vMRunWasmFromBuffer
  , vMRunWasmFromASTModule
  , vMAsyncRunWasmFromFile
  , vMAsyncRunWasmFromASTModule
  , vMAsyncRunWasmFromBuffer
  , vMRegisterModuleFromBuffer
  , vMRegisterModuleFromASTModule
  , vMRegisterModuleFromImport
  , vMLoadWasmFromFile
  , vMLoadWasmFromBuffer
  , vMLoadWasmFromASTModule
  , vMValidate
  , vMInstantiate
  , vMExecute
  , vMExecuteRegistered
  , vMAsyncExecute
  , vMAsyncExecuteRegistered
  , vMGetFunctionType
  , vMGetFunctionTypeRegistered
  , vMCleanup
  , vMGetFunctionListLength
  , vMGetFunctionList
  , vMGetImportModuleContext
  , vMGetActiveModule
  , vMGetRegisteredModule
  , vMListRegisteredModuleLength
  , vMListRegisteredModule
  , vMGetStoreContext
  , vMGetLoaderContext
  , vMGetValidatorContext
  , vMGetExecutorContext
  , vMGetStatisticsContext
  ) where

import WasmEdge.Internal.FFI.Bindings
    ( vMAsyncExecute,
      vMAsyncExecuteRegistered,
      vMAsyncRunWasmFromASTModule,
      vMAsyncRunWasmFromBuffer,
      vMAsyncRunWasmFromFile,
      vMCleanup,
      vMCreate,
      vMExecute,
      vMExecuteRegistered,
      vMGetActiveModule,
      vMGetExecutorContext,
      vMGetFunctionList,
      vMGetFunctionListLength,
      vMGetFunctionType,
      vMGetFunctionTypeRegistered,
      vMGetImportModuleContext,
      vMGetLoaderContext,
      vMGetRegisteredModule,
      vMGetStatisticsContext,
      vMGetStoreContext,
      vMGetValidatorContext,
      vMInstantiate,
      vMListRegisteredModule,
      vMListRegisteredModuleLength,
      vMLoadWasmFromASTModule,
      vMLoadWasmFromBuffer,
      vMLoadWasmFromFile,
      vMRegisterModuleFromASTModule,
      vMRegisterModuleFromBuffer,
      vMRegisterModuleFromFile,
      vMRegisterModuleFromImport,
      vMRunWasmFromASTModule,
      vMRunWasmFromBuffer,
      vMRunWasmFromFile,
      vMValidate,
      VMContext )
