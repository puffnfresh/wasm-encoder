module Language.Wasm.Encoder.Instruction (
  Instruction (..)
, putInstruction
) where

import Data.Int (Int32)
import Language.Wasm.Encoder.Types
import qualified Data.Serialize as S

data Instruction
  = Unreachable
  | Nop
  | Block BlockType
  | End
  | Call FuncIndex
  | ReturnCall FuncIndex
  | I32Const Int32
  | I32Add
  | LocalGet LocalIndex
  | LocalSet LocalIndex
  | RefCast RefType
  | StructGet TypeIndex FieldIndex
  | StructNew TypeIndex
  | RefNull HeapType
  | BrOnNull LabelIndex
  deriving (Eq, Ord, Show)

putInstruction
  :: Instruction
  -> S.Put
putInstruction Unreachable =
  S.putWord8 0x00
putInstruction Nop =
  S.putWord8 0x01
putInstruction (Block bt) = do
  S.putWord8 0x02
  putBlockType bt
putInstruction End =
  S.putWord8 0x0B
putInstruction (Call fi) = do
  S.putWord8 0x10
  putFuncIndex fi
putInstruction (ReturnCall fi) = do
  S.putWord8 0x12
  putFuncIndex fi
putInstruction (I32Const n) = do
  S.putWord8 0x41
  S.putWord8 (fromIntegral n)
putInstruction I32Add =
  S.putWord8 0x6A
putInstruction (LocalGet li) = do
  S.putWord8 0x20
  putLocalIndex li
putInstruction (LocalSet li) = do
  S.putWord8 0x21
  putLocalIndex li
putInstruction (RefCast (RefType n ht)) = do
  S.putWord8 0xFB
  S.putWord8 (case n of
    NotNullable ->
       0x16
    IsNullable ->
      0x17)
  putHeapType ht
putInstruction (StructGet ti fi) = do
  S.putWord8 0xFB
  S.putWord8 0x02
  putTypeIndex ti
  putFieldIndex fi
putInstruction (StructNew ti) = do
  S.putWord8 0xFB
  S.putWord8 0x00
  putTypeIndex ti
putInstruction (RefNull ht) = do
  S.putWord8 0xD0
  putHeapType ht
putInstruction (BrOnNull li) = do
  S.putWord8 0xD5
  putLabelIndex li
