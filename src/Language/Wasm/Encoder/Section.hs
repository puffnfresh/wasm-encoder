module Language.Wasm.Encoder.Section (
  Function (..)
, Section (..)
, header
, putCode
, putExpression
, putFunction
, putModule
, putSection
) where

import Data.Foldable (traverse_)
import Data.Word (Word8)
import Language.Wasm.Encoder.Instruction
import Language.Wasm.Encoder.Types
import qualified Data.ByteString as BS
import qualified Data.Serialize as S

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
        TypeSection cts ->
          (SectionIdType, putVec putRecType cts)
        FunctionSection tis ->
          (SectionIdFunction, putVec putTypeIndex tis)
        ExportSection es ->
          (SectionIdExport, putVec putExport es)
        CodeSection cs ->
          (SectionIdCode, putVec putCode cs)

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
  = TypeSection [RecType]
  | FunctionSection [TypeIndex]
  | ExportSection [Export]
  | CodeSection [Function]
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
