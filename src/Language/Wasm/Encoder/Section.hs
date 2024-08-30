module Language.Wasm.Encoder.Section (
  ConstInstruction (..)
, Data (..)
, DataMode (..)
, Element (..)
, ElementMode (..)
, ElementType (..)
, Function (..)
, Global (..)
, GlobalType (..)
, Section (..)
, Table (..)
, TableType (..)

, fromConstInstruction
, header
, putCode
, putData
, putDataMode
, putElement
, putElementMode
, putElementType
, putExpression
, putFunction
, putGlobal
, putGlobalType
, putModule
, putSection
, putTable
, putTableType
) where

import Data.Foldable (traverse_)
import Data.Word (Word8)
import Data.Int (Int32, Int64)
import Language.Wasm.Encoder.Instruction
import Language.Wasm.Encoder.Types
import qualified Data.ByteString as BS
import qualified Data.Serialize as S

data ElementMode
  = PassiveElement
  | ActiveElement TableIndex [ConstInstruction]
  | DeclarativeElement
  deriving (Eq, Ord, Show)

putElementMode
  :: ElementMode
  -> S.Put
putElementMode PassiveElement =
  pure ()
putElementMode (ActiveElement x e) = do
  putTableIndex x
  putConstExpression e
putElementMode DeclarativeElement =
  pure ()

data ElementType
  = FunctionsElement [FuncIndex]
  | ExpressionsElement RefType [[ConstInstruction]]
  deriving (Eq, Ord, Show)

putElementType
  :: ElementType
  -> S.Put
putElementType (FunctionsElement fi) = do
  S.putWord8 0x00
  putVec putFuncIndex fi
putElementType (ExpressionsElement rt cis) = do
  putRefType rt
  putVec putConstExpression cis

data Element
  = Element ElementMode ElementType
  deriving (Eq, Ord, Show)

putElement
  :: Element
  -> S.Put
putElement (Element m et) = do
  let
    modeBit =
      case m of
        PassiveElement -> 0x01
        ActiveElement _ _ -> 0x02
        DeclarativeElement -> 0x03
    typeBit =
      case et of
        FunctionsElement _ -> 0x00
        ExpressionsElement _ _ -> 0x04
  S.putWord8 (modeBit + typeBit)
  putElementMode m
  putElementType et

data ConstInstruction
  = ConstI32Const Int32
  | ConstI64Const Int64
  | ConstRefNull HeapType
  | ConstRefFunc FuncIndex
  | ConstGlobalGet GlobalIndex
  deriving (Eq, Ord, Show)

data DataMode
  = PassiveData
  | ActiveData MemIndex [ConstInstruction]
  deriving (Eq, Ord, Show)

putDataMode
  :: DataMode
  -> S.Put
putDataMode PassiveData =
  S.putWord8 0x01
putDataMode (ActiveData x e) = do
  S.putWord8 0x02
  putMemIndex x
  putConstExpression e

data Data
  = Data DataMode [Word8]
  deriving (Eq, Ord, Show)

putData
  :: Data
  -> S.Put
putData (Data dm bs) = do
  putDataMode dm
  putVec S.putWord8 bs

fromConstInstruction
  :: ConstInstruction
  -> Instruction
fromConstInstruction (ConstI32Const i) =
  I32Const i
fromConstInstruction (ConstI64Const i) =
  I64Const i
fromConstInstruction (ConstRefNull r) =
  RefNull r
fromConstInstruction (ConstRefFunc fi) =
  RefFunc fi
fromConstInstruction (ConstGlobalGet gi) =
  GlobalGet gi

putConstExpression
  :: [ConstInstruction]
  -> S.Put
putConstExpression =
  putExpression . fmap fromConstInstruction

putExpression
  :: [Instruction]
  -> S.Put
putExpression is = do
  traverse_ putInstruction is
  putInstruction End

data Function
  = Function [Locals] [Instruction]
  deriving (Eq, Ord, Show)

putFunction
  :: Function
  -> S.Put
putFunction (Function ls e) = do
  putVec putLocals ls
  putExpression e

data GlobalType
  = GlobalType ValType Mutable
  deriving (Eq, Ord, Show)

putGlobalType
  :: GlobalType
  -> S.Put
putGlobalType (GlobalType vt m) = do
  putValType vt
  putMutable m

data Global
  = Global GlobalType [Instruction]
  deriving (Eq, Ord, Show)

putGlobal
  :: Global
  -> S.Put
putGlobal (Global gt e) = do
  putGlobalType gt
  putExpression e

data TableType
  = TableType RefType Limits
  deriving (Eq, Ord, Show)

putTableType
  :: TableType
  -> S.Put
putTableType (TableType rt l) = do
  putRefType rt
  putLimits l

data Table
  = Table TableType
  | TableWithInit TableType [Instruction]
  deriving (Eq, Ord, Show)

putTable
  :: Table
  -> S.Put
putTable (Table tt) =
  putTableType tt
putTable (TableWithInit tt is) = do
  S.putWord8 0x40
  S.putWord8 0x00
  putTableType tt
  putExpression is

putCode
  :: Function
  -> S.Put
putCode =
  S.putNested (putULEB128 . fromIntegral) . putFunction

putSection
  :: Section
  -> S.Put
putSection s = do
  S.putWord8 (encodeSectionId i)
  S.putNested (putULEB128 . fromIntegral) p
  where
    (i, p) =
      case s of
        CustomSection n bs ->
          (SectionIdCustom, putName n *> S.putByteString (BS.pack bs))
        TypeSection cts ->
          (SectionIdType, putVec putRecType cts)
        ImportSection is ->
          (SectionIdImport, putVec putImport is)
        FunctionSection tis ->
          (SectionIdFunction, putVec putTypeIndex tis)
        TableSection ts ->
          (SectionIdTable, putVec putTable ts)
        MemorySection ls ->
          (SectionIdMemory, putVec putLimits ls)
        GlobalSection gs ->
          (SectionIdGlobal, putVec putGlobal gs)
        ExportSection es ->
          (SectionIdExport, putVec putExport es)
        StartSection fi ->
          (SectionIdStart, putFuncIndex fi)
        ElementSection es ->
          (SectionIdElement, putVec putElement es)
        CodeSection cs ->
          (SectionIdCode, putVec putCode cs)
        DataSection ds ->
          (SectionIdData, putVec putData ds)
        DataCountSection dc ->
          (SectionIdDataCount, putDataCount dc)
        TagSection ts ->
          (SectionIdTag, putVec putTag ts)

data SectionId
  = SectionIdCustom
  | SectionIdType
  | SectionIdImport
  | SectionIdFunction
  | SectionIdTable
  | SectionIdMemory
  | SectionIdGlobal
  | SectionIdExport
  | SectionIdStart
  | SectionIdElement
  | SectionIdCode
  | SectionIdData
  | SectionIdDataCount
  | SectionIdTag
  deriving (Eq, Ord, Show)

encodeSectionId :: SectionId -> Word8
encodeSectionId SectionIdCustom = 0
encodeSectionId SectionIdType = 1
encodeSectionId SectionIdImport = 2
encodeSectionId SectionIdFunction = 3
encodeSectionId SectionIdTable = 4
encodeSectionId SectionIdMemory = 5
encodeSectionId SectionIdGlobal = 6
encodeSectionId SectionIdExport = 7
encodeSectionId SectionIdStart = 8
encodeSectionId SectionIdElement = 9
encodeSectionId SectionIdCode = 10
encodeSectionId SectionIdData = 11
encodeSectionId SectionIdDataCount = 12
encodeSectionId SectionIdTag = 13

data Section
  = CustomSection BS.ByteString [Word8]
  | TypeSection [RecType]
  | ImportSection [Import]
  | FunctionSection [TypeIndex]
  | TableSection [Table]
  | MemorySection [Limits]
  | GlobalSection [Global]
  | ExportSection [Export]
  | StartSection FuncIndex
  | ElementSection [Element]
  | CodeSection [Function]
  | DataSection [Data]
  | DataCountSection DataCount
  | TagSection [Tag]
  deriving (Eq, Ord, Show)

header
  :: [Word8]
header =
  [ --       A     S     M
    0x00, 0x61, 0x73, 0x6D,
    -- Version
    0x01, 0x00, 0x00, 0x00
  ]

putModule
  :: [Section]
  -> S.Put
putModule sections = do
  S.putByteString (BS.pack header)
  traverse_ putSection sections
