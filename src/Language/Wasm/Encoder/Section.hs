module Language.Wasm.Encoder.Section (
  ConstInstruction (..)
, CustomSection (..)
, Data (..)
, DataMode (..)
, Element (..)
, ElementMode (..)
, ElementType (..)
, Function (..)
, Global (..)
, GlobalType (..)
, Module (..)
, Section (..)
, Table (..)
, TableType (..)

, emptyModule
, fromConstInstruction
, header
, noCustoms
, prependCustomSections
, putCode
, putCustomSection
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

putCustomSection
  :: CustomSection
  -> S.Put
putCustomSection (CustomSection n bs) = do
  S.putWord8 (encodeSectionId SectionIdCustom)
  S.putNested (putULEB128 . fromIntegral) (putName n *> S.putByteString (BS.pack bs))

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

data CustomSection
  = CustomSection BS.ByteString [Word8]
  deriving (Eq, Ord, Show)

data Section a
  = SectionMissing
  | Section a [CustomSection]
  deriving (Eq, Ord, Show)

noCustoms
  :: a
  -> Section a
noCustoms content =
  Section content []

putSection
  :: SectionId
  -> (a -> S.Put)
  -> Section a
  -> S.Put
putSection _ _ SectionMissing =
  pure ()
putSection i f (Section content customSections) = do
  S.putWord8 (encodeSectionId i)
  S.putNested (putULEB128 . fromIntegral) (f content)
  traverse_ putCustomSection customSections

data Module = Module
  { moduleCustomSectionsBefore :: [CustomSection]  -- Custom sections before any regular sections
  , moduleTypeSection :: Section [RecType]
  , moduleImportSection :: Section [Import]
  , moduleFunctionSection :: Section [TypeIndex]
  , moduleTableSection :: Section [Table]
  , moduleMemorySection :: Section [Limits]
  , moduleGlobalSection :: Section [Global]
  , moduleExportSection :: Section [Export]
  , moduleStartSection :: Section FuncIndex
  , moduleElementSection :: Section [Element]
  , moduleCodeSection :: Section [Function]
  , moduleDataSection :: Section [Data]
  , moduleDataCountSection :: Section DataCount
  , moduleTagSection :: Section [Tag]
  } deriving (Eq, Ord, Show)

emptyModule
  :: Module
emptyModule =
  Module
    { moduleCustomSectionsBefore = []
    , moduleTypeSection = SectionMissing
    , moduleImportSection = SectionMissing
    , moduleFunctionSection = SectionMissing
    , moduleTableSection = SectionMissing
    , moduleMemorySection = SectionMissing
    , moduleGlobalSection = SectionMissing
    , moduleExportSection = SectionMissing
    , moduleStartSection = SectionMissing
    , moduleElementSection = SectionMissing
    , moduleCodeSection = SectionMissing
    , moduleDataSection = SectionMissing
    , moduleDataCountSection = SectionMissing
    , moduleTagSection = SectionMissing
    }

prependCustomSections
  :: [CustomSection]
  -> Module
  -> Module
prependCustomSections customSections m =
  m { moduleCustomSectionsBefore = customSections <> moduleCustomSectionsBefore m }

header
  :: [Word8]
header =
  [ --       A     S     M
    0x00, 0x61, 0x73, 0x6D,
    -- Version
    0x01, 0x00, 0x00, 0x00
  ]

putModule
  :: Module
  -> S.Put
putModule m = do
  S.putByteString (BS.pack header)
  traverse_ putCustomSection (moduleCustomSectionsBefore m)
  putSection SectionIdType (putVec putRecType) (moduleTypeSection m)
  putSection SectionIdImport (putVec putImport) (moduleImportSection m)
  putSection SectionIdFunction (putVec putTypeIndex) (moduleFunctionSection m)
  putSection SectionIdTable (putVec putTable) (moduleTableSection m)
  putSection SectionIdMemory (putVec putLimits) (moduleMemorySection m)
  putSection SectionIdGlobal (putVec putGlobal) (moduleGlobalSection m)
  putSection SectionIdExport (putVec putExport) (moduleExportSection m)
  putSection SectionIdStart putFuncIndex (moduleStartSection m)
  putSection SectionIdElement (putVec putElement) (moduleElementSection m)
  putSection SectionIdCode (putVec putCode) (moduleCodeSection m)
  putSection SectionIdData (putVec putData) (moduleDataSection m)
  putSection SectionIdDataCount putDataCount (moduleDataCountSection m)
  putSection SectionIdTag (putVec putTag) (moduleTagSection m)
