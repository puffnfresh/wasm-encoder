module Language.Wasm.Encoder.Types (
  BlockType (..)
, CompType (..)
, Finality (..)
, RecType (..)
, Subtype (..)
, Export (..)
, ExportDesc (..)
, FieldIndex (..)
, FieldType (..)
, FuncIndex (..)
, GlobalIndex (..)
, HeapType (..)
, LabelIndex (..)
, LocalIndex (..)
, MemIndex (..)
, Mutable (..)
, Nullable (..)
, RefType (..)
, ResultType (..)
, StorageType (..)
, TypeIndex (..)
, ValType (..)
, Locals (..)
, putRecType
, putSubtype
, putBlockType
, putCompType
, putExport
, putExportDesc
, putFieldIndex
, putFieldType
, putFuncIndex
, putHeapType
, putLabelIndex
, putLocalIndex
, putMutable
, putRefType
, putStorageType
, putTypeIndex
, putValType
, putVec
, putLocals

, anyref
, finalNoRec

, putULEB128
) where

import Data.Foldable (traverse_)
import Data.Word (Word32)
import qualified Data.ByteString as BS
import qualified Data.Serialize as S

data HeapType
  = Func
  | Extern
  | Any
  | None
  | NoExtern
  | NoFunc
  | Eq
  | Struct
  | Array
  | I31
  | Exn
  | Concrete Word32
  deriving (Eq, Ord, Show)

putHeapType
  :: HeapType
  -> S.Put
putHeapType NoFunc =
  S.putWord8 0x73
putHeapType NoExtern =
  S.putWord8 0x72
putHeapType None =
  S.putWord8 0x71
putHeapType Func =
  S.putWord8 0x70
putHeapType Extern =
  S.putWord8 0x6F
putHeapType Any =
  S.putWord8 0x6E
putHeapType Eq =
  S.putWord8 0x6D
putHeapType I31 =
  S.putWord8 0x6C
putHeapType Struct =
  S.putWord8 0x6B
putHeapType Array =
  S.putWord8 0x6A
putHeapType Exn =
  S.putWord8 0x69
putHeapType (Concrete w) =
  S.putWord8 (fromIntegral w) -- LEB128 S33

data Nullable
  = NotNullable
  | IsNullable
  deriving (Eq, Ord, Show)

data RefType
  = RefType Nullable HeapType
  deriving (Eq, Ord, Show)

putRefType
  :: RefType
  -> S.Put
putRefType (RefType n ht) = do
  S.putWord8 (case n of
    NotNullable -> 0x64
    IsNullable -> 0x63)
  putHeapType ht

data ValType
  = I32
  | I64
  | F32
  | F64
  | V128
  | Ref RefType
  deriving (Eq, Ord, Show)

putValType
  :: ValType
  -> S.Put
putValType I32 =
  S.putWord8 0x7F
putValType I64 =
   S.putWord8 0x7E
putValType F32 =
   S.putWord8 0x7D
putValType F64 =
   S.putWord8 0x7C
putValType V128 =
  S.putWord8 0x7B
putValType (Ref rt) =
  putRefType rt

data StorageType
  = I8
  | I16
  | Val ValType
  deriving (Eq, Ord, Show)

putStorageType
  :: StorageType
  -> S.Put
putStorageType I8 =
  S.putWord8 0x78
putStorageType I16 =
  S.putWord8 0x77
putStorageType (Val vt) =
  putValType vt

data Mutable
  = MutConst
  | MutVar
  deriving (Eq, Ord, Show)

putMutable
  :: Mutable
  -> S.Put
putMutable m =
  S.putWord8 (case m of
    MutConst -> 0
    MutVar -> 1)

data FieldType
  = FieldType Mutable StorageType
  deriving (Eq, Ord, Show)

putFieldType
  :: FieldType
  -> S.Put
putFieldType (FieldType m st) = do
  putStorageType st
  putMutable m

data ResultType
  = ResultType [ValType]
  deriving (Eq, Ord, Show)

putResultType
  :: ResultType
  -> S.Put
putResultType (ResultType v) =
  putVec putValType v

data Finality
  = IsFinal
  | NotFinal
  deriving (Eq, Ord, Show)

data Subtype
  = Subtype Finality (Maybe TypeIndex) CompType
  deriving (Eq, Ord, Show)

putSubtype
  :: Subtype
  -> S.Put
putSubtype (Subtype NotFinal (Just s) c) = do
  S.putWord8 0x50
  putVec putTypeIndex [s]
  putCompType c
putSubtype (Subtype IsFinal (Just s) c) = do
  S.putWord8 0x4F
  putVec putTypeIndex [s]
  putCompType c
putSubtype (Subtype _ Nothing c) =
  putCompType c

data RecType
  = Rec [Subtype]
  | NoRec Subtype
  deriving (Eq, Ord, Show)

finalNoRec
  :: CompType
  -> RecType
finalNoRec =
  NoRec . Subtype IsFinal Nothing

putRecType
  :: RecType
  -> S.Put
putRecType (Rec ss) = do
  S.putWord8 0x4E
  putVec putSubtype ss
putRecType (NoRec s) = do
  putSubtype s

data CompType
  = ArrayType FieldType
  | StructType [FieldType]
  | FuncType ResultType ResultType
  deriving (Eq, Ord, Show)

putCompType
  :: CompType
  -> S.Put
putCompType (ArrayType ft) = do
  S.putWord8 0x5E
  putFieldType ft
putCompType (StructType fts) = do
  S.putWord8 0x5F
  putVec putFieldType fts
putCompType (FuncType rt1 rt2) = do
  S.putWord8 0x60
  putResultType rt1
  putResultType rt2

-- data RecType
--   = RecType
--   deriving (Eq, Ord, Show)

putVec
  :: (a -> S.Put)
  -> [a]
  -> S.Put
putVec f as = do
  putULEB128 (fromIntegral (length as))
  traverse_ f as

newtype TypeIndex
  = TypeIndex Word32
  deriving (Eq, Ord, Show)

putTypeIndex
  :: TypeIndex
  -> S.Put
putTypeIndex (TypeIndex n) =
  S.putWord8 (fromIntegral n)

newtype FieldIndex
  = FieldIndex Word32
  deriving (Eq, Ord, Show)

putFieldIndex
  :: FieldIndex
  -> S.Put
putFieldIndex (FieldIndex n) =
  S.putWord8 (fromIntegral n)

newtype FuncIndex
  = FuncIndex Word32
  deriving (Eq, Ord, Show)

putFuncIndex
  :: FuncIndex
  -> S.Put
putFuncIndex (FuncIndex n) =
  S.putWord8 (fromIntegral n)

data LocalIndex
  = LocalIndex Word32
  deriving (Eq, Ord, Show)

putLocalIndex
  :: LocalIndex
  -> S.Put
putLocalIndex (LocalIndex n) =
  S.putWord8 (fromIntegral n)

data MemIndex
  = MemIndex Word32
  deriving (Eq, Ord, Show)

putMemIndex
  :: MemIndex
  -> S.Put
putMemIndex (MemIndex n) =
  S.putWord8 (fromIntegral n)

data GlobalIndex
  = GlobalIndex Word32
  deriving (Eq, Ord, Show)

putGlobalIndex
  :: GlobalIndex
  -> S.Put
putGlobalIndex (GlobalIndex n) =
  S.putWord8 (fromIntegral n)

data LabelIndex
  = LabelIndex Word32
  deriving (Eq, Ord, Show)

putLabelIndex
  :: LabelIndex
  -> S.Put
putLabelIndex (LabelIndex n) =
  S.putWord8 (fromIntegral n)

data ExportDesc
  = ExportFuncIndex FuncIndex
  | ExportTypeIndex TypeIndex
  | ExportMemIndex MemIndex
  | ExportGlobalIndex GlobalIndex
  deriving (Eq, Ord, Show)

putExportDesc
  :: ExportDesc
  -> S.Put
putExportDesc (ExportFuncIndex ti) = do
  S.putWord8 0x00
  putFuncIndex ti
putExportDesc (ExportTypeIndex ti) = do
  S.putWord8 0x01
  putTypeIndex ti
putExportDesc (ExportMemIndex mi) = do
  S.putWord8 0x02
  putMemIndex mi
putExportDesc (ExportGlobalIndex gi) = do
  S.putWord8 0x03
  putGlobalIndex gi

data Export
  = Export BS.ByteString ExportDesc
  deriving (Eq, Ord, Show)

putExport
  :: Export
  -> S.Put
putExport (Export n ed) = do
  putULEB128 (fromIntegral (BS.length n))
  S.putByteString n
  putExportDesc ed

data Locals
  = Locals Word32 ValType
  deriving (Eq, Ord, Show)

putLocals
  :: Locals
  -> S.Put
putLocals (Locals n vt) = do
  putULEB128 n
  putValType vt

data BlockType
  = EmptyBlockType
  deriving (Eq, Ord, Show)

putBlockType
  :: BlockType
  -> S.Put
putBlockType EmptyBlockType =
  S.putWord8 0x40

putULEB128
  :: Word32
  -> S.Put
putULEB128 =
  S.putWord8 . fromIntegral

anyref
  :: ValType
anyref =
  Ref (RefType IsNullable Any)
