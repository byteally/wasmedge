{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module WasmEdge.Internal.FFI.ValueTypes
  ( i32Value
  , deleteString
  , wasmStringEq
  , wasmStringLength
  , toText
  , mkStringFromBytes
  , stringCopy
  , configureAddHostRegistration
  , logSetErrorLevel
  , logSetDebugLevel
  , ConfigureContext
  , ProgramOptionType (..)
  , Proposal (..)
  , HostRegistration (..)
  , CompilerOptimizationLevel (..)
  , CompilerOutputFormat (..)
  , ErrCategory (..)
  , ErrCode (..)
  , ValType (..)
  , NumType (..)
  , RefType (..)
  , ExternalType (..)
  , Mutability (..)
  , WasmString
  ) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Word
-- import Foreign
import Foreign.C
-- import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (Storable (..))
-- import GHC.Ptr
import System.IO.Unsafe
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

-- import Data.Vector.Storable.Mutable (IOVector)
-- import qualified Data.Vector.Storable.Mutable as VSM

#include "wasmedge/wasmedge.h"
#include <stdio.h>

{#context prefix = "WasmEdge"#}

{#fun pure unsafe WasmEdge_ValueGenI32 as i32Value
   { `Int32'
   } -> `()'
#}

#c

void StringCreateByBufferOut(WasmEdge_String* strOut, const char *Str, const uint32_t Len)
{
  *strOut = WasmEdge_StringCreateByBuffer(Str, Len);
}

WasmEdge_String StringWrapOut(WasmEdge_String* strOut, const char *Buf, const uint32_t Len)
{
  *strOut = WasmEdge_StringWrap(Buf, Len);
}

void StringDeleteByPtr(WasmEdge_String* Str)
{
  printf("string is getting deleted\n");
  WasmEdge_StringDelete(*Str);
}

void ResultGenOut(WasmEdge_Result* out, const enum WasmEdge_ErrCategory Category, const uint32_t Code)
{
  *out = WasmEdge_ResultGen(Category, Code);
}

void C_Result_Success(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Success.Code;}
void C_Result_Terminate(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Terminate.Code;}
void C_Result_Fail(WasmEdge_Result* res) { res->Code = WasmEdge_Result_Fail.Code;}
#endc

-- {#pointer *WasmEdge_Value as WasmValue foreign newtype #}
{#pointer *WasmEdge_String as WasmString foreign finalizer StringDeleteByPtr as deleteString newtype #}
{#pointer *WasmEdge_Result as WasmResult foreign newtype #}
{#pointer *WasmEdge_Limit as Limit foreign newtype #}
-- Program option for plugins.
{#pointer *WasmEdge_ProgramOption as ProgramOption foreign newtype #}
-- Module descriptor for plugins.
{#pointer *WasmEdge_ModuleDescriptor as ModuleDescriptor foreign newtype #}
-- Version data for plugins.
{#pointer *WasmEdge_PluginVersionData as PluginVersionData foreign newtype #}
-- Plugin descriptor for plugins.
{#pointer *WasmEdge_PluginDescriptor as PluginDescriptor foreign newtype #}


{#fun unsafe StringCreateByBufferOut as mkStringFromBytesIO {+, useAsCStringLenBS*`ByteString'& } -> `WasmString' #}
{#fun pure unsafe StringWrapOut as stringWrap {+, useAsCStringLenBS*`ByteString'&} -> `WasmString' #}
{#fun pure unsafe WasmEdge_StringIsEqual as wasmStringEq {%`WasmString', %`WasmString'} -> `Bool' #}
{#fun pure unsafe WasmEdge_StringCopy as _stringCopy {%`WasmString', memBuffIn*`MemBuff'&} -> `Word32' #}
{#fun pure unsafe C_Result_Success as mkResultSuccess {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Terminate as mkResultTerminate {+} -> `WasmResult' #}
{#fun pure unsafe C_Result_Fail as mkResultFail {+} -> `WasmResult' #}

mkStringFromBytes :: ByteString -> WasmString
mkStringFromBytes bs = unsafePerformIO $ do
  ws@(WasmString fp) <- mkStringFromBytesIO bs
  addForeignPtrFinalizer deleteString fp
  pure ws

useAsCStringLenBS :: ByteString -> ((CString, CUInt) -> IO a) -> IO a
useAsCStringLenBS bs f = BS.useAsCStringLen bs (\strLen -> f (fromIntegral <$> strLen))

_packCStringLenBS :: CString -> CUInt -> IO ByteString
_packCStringLenBS cstr len = BS.packCStringLen (cstr, fromIntegral len)

packCStringBS :: CString -> IO ByteString
packCStringBS cstr = BS.packCString cstr

memBuffIn :: MemBuff -> ((Ptr CChar, CUInt) -> IO a) -> IO a
memBuffIn mem f = withForeignPtr (memBuff mem) $ \p -> f (p, fromIntegral (memBuffLen mem))

data MemBuff = MemBuff {memBuffLen :: Int, memBuff :: ForeignPtr CChar}

allocMemBuff :: Int -> IO MemBuff
allocMemBuff sz = MemBuff sz <$> mallocForeignPtrBytes sz

stringCopy :: Word32 -> WasmString -> ByteString
stringCopy sz wstr = unsafePerformIO $ do
  mem <- allocMemBuff (fromIntegral sz)
  let cpLen = _stringCopy wstr mem
  withForeignPtr (memBuff mem) $ \p -> BS.packCStringLen (p, fromIntegral cpLen)

instance Eq WasmString where
  (==) = wasmStringEq

instance IsString WasmString where
  fromString = mkStringFromBytes . Char8.pack

instance Eq WasmResult where
  wr1 == wr2 = unsafePerformIO $ withWasmResult wr1 $ \wrp1 ->
    withWasmResult wr2 $ \wrp2 -> do
    r1 <- {#get WasmEdge_Result.Code #} wrp1
    r2 <- {#get WasmEdge_Result.Code #} wrp2
    pure $ r1 == r2
    
pattern WRSuccess :: WasmResult
pattern WRSuccess <- ((mkResultSuccess ==) -> True) where
  WRSuccess = mkResultSuccess

pattern WRTerminate :: WasmResult
pattern WRTerminate <- ((mkResultTerminate ==) -> True) where
  WRTerminate = mkResultTerminate

pattern WRFail :: WasmResult
pattern WRFail <- ((mkResultFail ==) -> True) where
  WRFail = mkResultFail

{-# COMPLETE WRSuccess, WRTerminate, WRFail #-}  

wasmStringLength :: WasmString -> IO Word32
wasmStringLength wstr = withWasmString wstr (fmap fromIntegral . {#get WasmEdge_String.Length #})

toText :: WasmString -> Text
toText wstr = unsafePerformIO $ withWasmString wstr $ \p -> do
  cstr <- {#get WasmEdge_String.Buf #} p
  cstrLen <- {#get WasmEdge_String.Length #} p
  T.peekCStringLen (cstr, fromIntegral cstrLen)

instance Show WasmString where
  show = T.unpack . toText

cToEnum :: Enum a => CInt -> a
cToEnum = toEnum . fromIntegral

cFromEnum :: Enum a => a -> CInt
cFromEnum = fromIntegral . fromEnum

{#fun pure unsafe ResultGenOut as resultGen {+, cFromEnum`ErrCategory', `CUInt'} -> `WasmResult' #}
{#fun pure unsafe WasmEdge_ResultGetCode as getResultCode {%`WasmResult'} -> `Word32' #}
{#fun pure unsafe WasmEdge_ResultGetCategory as getResultCategory {%`WasmResult'} -> `ErrCategory'cToEnum #}
{#fun pure unsafe WasmEdge_ResultGetMessage as getResultMessage {%`WasmResult'} -> `ByteString'packCStringBS* #}
{#fun pure unsafe WasmEdge_LimitIsEqual as limitEq_ {%`Limit',%`Limit'} -> `Bool'#}

instance Eq Limit where
  (==) = limitEq_

{#pointer *ConfigureContext as ^ foreign finalizer ConfigureDelete as ^ newtype #}
{#pointer *StatisticsContext as ^ foreign finalizer StatisticsDelete as ^ newtype #}
{#pointer *ASTModuleContext as ^ foreign finalizer ASTModuleDelete as ^ newtype #}
{#pointer *FunctionTypeContext as ^ foreign finalizer FunctionTypeDelete as ^ newtype #}
{#pointer *MemoryTypeContext as ^ foreign finalizer MemoryTypeDelete as ^ newtype #}
{#pointer *TableTypeContext as ^ foreign finalizer TableTypeDelete as ^ newtype #}
{#pointer *GlobalTypeContext as ^ foreign finalizer GlobalTypeDelete as ^ newtype #}
{#pointer *ImportTypeContext as ^ foreign newtype #}
{#pointer *ExportTypeContext as ^ foreign newtype #}
{#pointer *CompilerContext as ^ foreign finalizer CompilerDelete as ^ newtype #}
{#pointer *LoaderContext as ^ foreign finalizer LoaderDelete as ^ newtype #}
{#pointer *ValidatorContext as ^ foreign finalizer ValidatorDelete as ^ newtype #}
{#pointer *ExecutorContext as ^ foreign finalizer ExecutorDelete as ^ newtype #}
{#pointer *StoreContext as ^ foreign finalizer StoreDelete as ^ newtype #}
{#pointer *ModuleInstanceContext as ^ foreign finalizer ModuleInstanceDelete as ^ newtype #}
{#pointer *FunctionInstanceContext as ^ foreign finalizer FunctionInstanceDelete as ^ newtype #}
{#pointer *TableInstanceContext as ^ foreign finalizer TableInstanceDelete as ^ newtype #}
{#pointer *MemoryInstanceContext as ^ foreign finalizer MemoryInstanceDelete as ^ newtype #}
{#pointer *GlobalInstanceContext as ^ foreign finalizer GlobalInstanceDelete as ^ newtype #}
{#pointer *CallingFrameContext as ^ foreign newtype #}
{#pointer *Async as ^ foreign finalizer AsyncDelete as ^ newtype #}
{#pointer *VMContext as ^ foreign finalizer VMDelete as ^ newtype #}
{#pointer *PluginContext as ^ foreign newtype #}

{#enum ProgramOptionType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WasmEdge logging functions
{-|
  Set the logging system to filter to error level.
-}
{#fun unsafe LogSetErrorLevel as ^ {} -> `()'#}

{-|
  Set the logging system to filter to debug level.
-}
{#fun unsafe LogSetDebugLevel as ^ {} -> `()'#}

-- WASM Proposal C enumeration.
{#enum Proposal as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Host Module Registration C enumeration.
{#enum HostRegistration as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- AOT compiler optimization level C enumeration.
{#enum CompilerOptimizationLevel as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- AOT compiler output binary format C enumeration.
{#enum CompilerOutputFormat as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Error category C enumeration.
{#enum ErrCategory as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Error code C enumeration.
{#enum ErrCode as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Value type C enumeration.
{#enum ValType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq)
#}
deriving via ViaFromEnum ValType instance Storable ValType

-- WASM Number type C enumeration.
{#enum NumType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Reference type C enumeration.
{#enum RefType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM Mutability C enumeration.
{#enum Mutability as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- WASM External type C enumeration.
{#enum ExternalType as ^ {}
  with prefix = "WasmEdge_"
  deriving (Show, Eq) #}

-- Configure
{#fun unsafe ConfigureCreate as ^ {} -> `ConfigureContext'#}
{#fun unsafe ConfigureAddProposal as ^ {`ConfigureContext',`Proposal'} -> `()'#}
{#fun unsafe ConfigureRemoveProposal as ^ {`ConfigureContext',`Proposal'} -> `()'#}
{#fun unsafe ConfigureHasProposal as ^ {`ConfigureContext',`Proposal'} -> `Bool'#}
{#fun unsafe ConfigureAddHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `()'#}
{#fun unsafe ConfigureRemoveHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `()'#}
{#fun unsafe ConfigureHasHostRegistration as ^ {`ConfigureContext',`HostRegistration'} -> `Bool'#}
{#fun unsafe ConfigureSetMaxMemoryPage as ^ {`ConfigureContext', `Word32'} -> `()'#}
{#fun unsafe ConfigureGetMaxMemoryPage as ^ {`ConfigureContext'} -> `Word32'#}
{#fun unsafe ConfigureSetForceInterpreter as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureIsForceInterpreter as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureCompilerSetOptimizationLevel as ^ {`ConfigureContext', `CompilerOptimizationLevel'} -> `()'#}
{#fun unsafe ConfigureCompilerGetOptimizationLevel as ^ {`ConfigureContext'} -> `CompilerOptimizationLevel'#}
{#fun unsafe ConfigureCompilerSetOutputFormat as ^ {`ConfigureContext', `CompilerOutputFormat'} -> `()'#}
{#fun unsafe ConfigureCompilerGetOutputFormat as ^ {`ConfigureContext'} -> `CompilerOutputFormat'#}
{#fun unsafe ConfigureCompilerSetDumpIR as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureCompilerIsDumpIR as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureCompilerSetGenericBinary as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureCompilerIsGenericBinary as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureCompilerSetInterruptible as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureCompilerIsInterruptible as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureStatisticsSetInstructionCounting as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureStatisticsIsInstructionCounting as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureStatisticsSetCostMeasuring as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureStatisticsIsCostMeasuring as ^ {`ConfigureContext'} -> `Bool'#}
{#fun unsafe ConfigureStatisticsSetTimeMeasuring as ^ {`ConfigureContext', `Bool'} -> `()'#}
{#fun unsafe ConfigureStatisticsIsTimeMeasuring as ^ {`ConfigureContext'} -> `Bool'#}

-- Statistics
{#fun unsafe StatisticsCreate as ^ {} -> `StatisticsContext'#}
{#fun unsafe StatisticsGetInstrCount as ^ {`StatisticsContext'} -> `Word64'#}
{#fun unsafe StatisticsGetInstrPerSecond as ^ {`StatisticsContext'} -> `Double'#}
{#fun unsafe StatisticsGetTotalCost as ^ {`StatisticsContext'} -> `Word64'#}
-- TODO:
-- {#fun unsafe StatisticsSetCostTable as ^ {`StatisticsContext', `MemBuff'} -> `()'#}
{#fun unsafe StatisticsSetCostLimit as ^ {`StatisticsContext', `Word64'} -> `()'#}
{#fun unsafe StatisticsClear as ^ {`StatisticsContext'} -> `()'#}

-- * Module
-- TODO:
-- {#fun unsafe ASTModuleListImports as ^ {`ASTModuleContext'} -> `()'#}
{#fun unsafe ASTModuleListExportsLength as ^ {`ASTModuleContext'} -> `Word32'#}
-- TODO:
-- {#fun unsafe ASTModuleListExports as ^ {`ASTModuleContext'} -> `()'#}

-- * Function
{#fun unsafe FunctionTypeCreate as ^ {fromIOVecOr0Ptr*`Vector ValType'&, fromIOVecOr0Ptr*`Vector ValType'&} -> `FunctionTypeContext'#}
{#fun unsafe FunctionTypeGetParametersLength as ^ {`FunctionTypeContext'} -> `Word32'#}
-- {#fun unsafe FunctionTypeGetParameters as ^ {`FunctionTypeContext', +} -> `(Vector ValType'#}


fromIOVecOr0Ptr :: Vector ValType -> ((Ptr CInt, CUInt) -> IO b) -> IO b
fromIOVecOr0Ptr v f
  | VS.null v = f (nullPtr, 0)
  | otherwise = VS.unsafeWith v $ \p -> f (castPtr p, fromIntegral $ VS.length v)

newtype ViaFromEnum t = ViaFromEnum {getHsEnumTy :: t}

instance Enum t => Storable (ViaFromEnum t) where
  sizeOf = sizeOf . fromEnum . getHsEnumTy
  alignment = alignment . fromEnum . getHsEnumTy
  peek = fmap (ViaFromEnum . toEnum) . peek @Int . castPtr 
  poke p v = poke @Int (castPtr p) (fromEnum $ getHsEnumTy v)
