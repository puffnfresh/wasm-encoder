module Language.Wasm.Encoder.Instruction (
  Instruction (..)
, putInstruction
) where

import Data.Int (Int32, Int64)
import Data.Word (Word32)
import Language.Wasm.Encoder.Types
import qualified Data.Serialize as S

data Instruction
  = Unreachable
  | Nop
  | Block BlockType
  | Loop BlockType
  | If BlockType
  | Else
  | End
  | Br LabelIndex
  | BrIf LabelIndex
  | BrTable [LabelIndex] LabelIndex
  | Return
  | Call FuncIndex
  | CallIndirect TableIndex TypeIndex
  | ReturnCall FuncIndex
  | ReturnCallIndirect TableIndex TypeIndex
  | CallRef TypeIndex
  | Drop
  | Select (Maybe ValType)
  | LocalGet LocalIndex
  | LocalSet LocalIndex
  | LocalTee LocalIndex
  | GlobalGet GlobalIndex
  | GlobalSet GlobalIndex
  | TableGet TableIndex
  | TableSet TableIndex
  | I32Load MemArg
  | I64Load MemArg
  | F32Load MemArg
  | F64Load MemArg
  | I32Load8S MemArg
  | I32Load8U MemArg
  | I32Load16S MemArg
  | I32Load16U MemArg
  | I64Load8S MemArg
  | I64Load8U MemArg
  | I64Load16S MemArg
  | I64Load16U MemArg
  | I64Load32S MemArg
  | I64Load32U MemArg
  | I32Store MemArg
  | I64Store MemArg
  | F32Store MemArg
  | F64Store MemArg
  | I32Store8 MemArg
  | I32Store16 MemArg
  | I64Store8 MemArg
  | I64Store16 MemArg
  | I64Store32 MemArg
  | MemorySize
  | MemoryGrow
  | I32Const Int32
  | I64Const Int64
  -- | F32Const Float
  -- | F64Const Double
  | I32Eqz
  | I32Eq
  | I32Ne
  | I32LtS
  | I32LtU
  | I32GtS
  | I32GtU
  | I32LeS
  | I32LeU
  | I32GeS
  | I32GeU
  | I64Eqz
  | I64Eq
  | I64Ne
  | I64LtS
  | I64LtU
  | I64GtS
  | I64GtU
  | I64LeS
  | I64LeU
  | I64GeS
  | I64GeU
  | F32Eq
  | F32Ne
  | F32Lt
  | F32Gt
  | F32Le
  | F32Ge
  | F64Eq
  | F64Ne
  | F64Lt
  | F64Gt
  | F64Le
  | F64Ge
  | I32Clz
  | I32Ctz
  | I32Popcnt
  | I32Add
  | I32Sub
  | I32Mul
  | I32DivS
  | I32DivU
  | I32RemS
  | I32RemU
  | I32And
  | I32Or
  | I32Xor
  | I32Shl
  | I32ShrS
  | I32ShrU
  | I32Rotl
  | I32Rotr
  | I64Clz
  | I64Ctz
  | I64Popcnt
  | I64Add
  | I64Sub
  | I64Mul
  | I64DivS
  | I64DivU
  | I64RemS
  | I64RemU
  | I64And
  | I64Or
  | I64Xor
  | I64Shl
  | I64ShrS
  | I64ShrU
  | I64Rotl
  | I64Rotr
  | F32Abs
  | F32Neg
  | F32Ceil
  | F32Floor
  | F32Trunc
  | F32Nearest
  | F32Sqrt
  | F32Add
  | F32Sub
  | F32Mul
  | F32Div
  | F32Min
  | F32Max
  | F32Copysign
  | F64Abs
  | F64Neg
  | F64Ceil
  | F64Floor
  | F64Trunc
  | F64Nearest
  | F64Sqrt
  | F64Add
  | F64Sub
  | F64Mul
  | F64Div
  | F64Min
  | F64Max
  | F64Copysign
  | I32WrapI64
  | I32TruncF32S
  | I32TruncF32U
  | I32TruncF64S
  | I32TruncF64U
  | I64ExtendI32S
  | I64ExtendI32U
  | I64TruncF32S
  | I64TruncF32U
  | I64TruncF64S
  | I64TruncF64U
  | F32ConvertI32S
  | F32ConvertI32U
  | F32ConvertI64S
  | F32ConvertI64U
  | F32DemoteF64
  | F64ConvertI32S
  | F64ConvertI32U
  | F64ConvertI64S
  | F64ConvertI64U
  | F64PromoteF32
  | I32ReinterpretF32
  | I64ReinterpretF64
  | F32ReinterpretI32
  | F64ReinterpretI64
  | I32Extend8S
  | I32Extend16S
  | I64Extend8S
  | I64Extend16S
  | I64Extend32S
  | RefNull HeapType
  | RefIsNull
  | RefFunc FuncIndex
  | RefEq
  | RefAsNonNull
  | BrOnNull LabelIndex
  | BrOnNonNull LabelIndex
  | StructNew TypeIndex
  | StructNewDefault TypeIndex
  | StructGet TypeIndex FieldIndex
  | StructGetS TypeIndex FieldIndex
  | StructGetU TypeIndex FieldIndex
  | StructSet TypeIndex FieldIndex
  | ArrayNew TypeIndex
  | ArrayNewFixed TypeIndex Word32
  | ArrayNewDefault TypeIndex
  -- | ArrayNewData TypeIndex DataIndex
  -- | ArrayNewData TypeIndex ElemIndex
  | ArrayGet TypeIndex
  | ArrayGetS TypeIndex
  | ArrayGetU TypeIndex
  | ArraySet TypeIndex
  | ArrayLen
  | ArrayFill TypeIndex
  | ArrayCopy TypeIndex TypeIndex
  -- | ArrayInitData TypeIndex DataIndex
  -- | ArrayInitElem TypeIndex ElemIndex
  | RefTest RefType
  | RefCast RefType
  | AnyConvertExtern
  | ExternConvertAny
  | RefI31
  | I31GetS
  | I31GetU
  | V128Load MemArg
  | V128Store MemArg
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
putInstruction (Loop bt) = do
  S.putWord8 0x03
  putBlockType bt
putInstruction (If bt) = do
  S.putWord8 0x04
  putBlockType bt
putInstruction Else =
  S.putWord8 0x05
putInstruction End =
  S.putWord8 0x0B
putInstruction (Br li) = do
  S.putWord8 0x0C
  putLabelIndex li
putInstruction (BrIf li) = do
  S.putWord8 0x0D
  putLabelIndex li
putInstruction (BrTable lis li) = do
  S.putWord8 0x0E
  putVec putLabelIndex lis
  putLabelIndex li
putInstruction Return =
  S.putWord8 0x0F
putInstruction (Call fi) = do
  S.putWord8 0x10
  putFuncIndex fi
putInstruction (CallIndirect tai tyi) = do
  S.putWord8 0x11
  putTableIndex tai
  putTypeIndex tyi
putInstruction (ReturnCall fi) = do
  S.putWord8 0x12
  putFuncIndex fi
putInstruction (ReturnCallIndirect tai tyi) = do
  S.putWord8 0x13
  putTableIndex tai
  putTypeIndex tyi
putInstruction (CallRef ti) = do
  S.putWord8 0x14
  putTypeIndex ti
putInstruction Drop =
  S.putWord8 0x1A
putInstruction (Select Nothing) =
  S.putWord8 0x1B
putInstruction (Select (Just vt)) = do
  S.putWord8 0x1C
  putValType vt
putInstruction (LocalGet li) = do
  S.putWord8 0x20
  putLocalIndex li
putInstruction (LocalSet li) = do
  S.putWord8 0x21
  putLocalIndex li
putInstruction (LocalTee li) = do
  S.putWord8 0x22
  putLocalIndex li
putInstruction (GlobalGet gi) = do
  S.putWord8 0x23
  putGlobalIndex gi
putInstruction (GlobalSet gi) = do
  S.putWord8 0x24
  putGlobalIndex gi
putInstruction (TableGet ti) = do
  S.putWord8 0x25
  putTableIndex ti
putInstruction (TableSet ti) = do
  S.putWord8 0x26
  putTableIndex ti
putInstruction (I32Load ma) = do
  S.putWord8 0x28
  putMemArg ma
putInstruction (I64Load ma) = do
  S.putWord8 0x29
  putMemArg ma
putInstruction (F32Load ma) = do
  S.putWord8 0x2A
  putMemArg ma
putInstruction (F64Load ma) = do
  S.putWord8 0x2B
  putMemArg ma
putInstruction (I32Load8S ma) = do
  S.putWord8 0x2C
  putMemArg ma
putInstruction (I32Load8U ma) = do
  S.putWord8 0x2D
  putMemArg ma
putInstruction (I32Load16S ma) = do
  S.putWord8 0x2E
  putMemArg ma
putInstruction (I32Load16U ma) = do
  S.putWord8 0x2F
  putMemArg ma
putInstruction (I64Load8S ma) = do
  S.putWord8 0x30
  putMemArg ma
putInstruction (I64Load8U ma) = do
  S.putWord8 0x31
  putMemArg ma
putInstruction (I64Load16S ma) = do
  S.putWord8 0x32
  putMemArg ma
putInstruction (I64Load16U ma) = do
  S.putWord8 0x33
  putMemArg ma
putInstruction (I64Load32S ma) = do
  S.putWord8 0x34
  putMemArg ma
putInstruction (I64Load32U ma) = do
  S.putWord8 0x35
  putMemArg ma
putInstruction (I32Store ma) = do
  S.putWord8 0x36
  putMemArg ma
putInstruction (I64Store ma) = do
  S.putWord8 0x37
  putMemArg ma
putInstruction (F32Store ma) = do
  S.putWord8 0x38
  putMemArg ma
putInstruction (F64Store ma) = do
  S.putWord8 0x39
  putMemArg ma
putInstruction (I32Store8 ma) = do
  S.putWord8 0x3A
  putMemArg ma
putInstruction (I32Store16 ma) = do
  S.putWord8 0x3B
  putMemArg ma
putInstruction (I64Store8 ma) = do
  S.putWord8 0x3C
  putMemArg ma
putInstruction (I64Store16 ma) = do
  S.putWord8 0x3D
  putMemArg ma
putInstruction (I64Store32 ma) = do
  S.putWord8 0x3E
  putMemArg ma
putInstruction MemorySize = do
  S.putWord8 0x3F
  S.putWord8 0x00
putInstruction MemoryGrow = do
  S.putWord8 0x40
  S.putWord8 0x00
putInstruction (I32Const n) = do
  S.putWord8 0x41
  S.putWord8 (fromIntegral n)
putInstruction (I64Const n) = do
  S.putWord8 0x41
  S.putInt64le n
putInstruction I32Eqz =
  S.putWord8 0x45
putInstruction I32Eq =
  S.putWord8 0x46
putInstruction I32Ne =
  S.putWord8 0x47
putInstruction I32LtS =
  S.putWord8 0x48
putInstruction I32LtU =
  S.putWord8 0x49
putInstruction I32GtS =
  S.putWord8 0x4A
putInstruction I32GtU =
  S.putWord8 0x4B
putInstruction I32LeS =
  S.putWord8 0x4C
putInstruction I32LeU =
  S.putWord8 0x4D
putInstruction I32GeS =
  S.putWord8 0x4E
putInstruction I32GeU =
  S.putWord8 0x4F
putInstruction I64Eqz =
  S.putWord8 0x50
putInstruction I64Eq =
  S.putWord8 0x51
putInstruction I64Ne =
  S.putWord8 0x52
putInstruction I64LtS =
  S.putWord8 0x53
putInstruction I64LtU =
  S.putWord8 0x54
putInstruction I64GtS =
  S.putWord8 0x55
putInstruction I64GtU =
  S.putWord8 0x56
putInstruction I64LeS =
  S.putWord8 0x57
putInstruction I64LeU =
  S.putWord8 0x58
putInstruction I64GeS =
  S.putWord8 0x59
putInstruction I64GeU =
  S.putWord8 0x5A
putInstruction F32Eq =
  S.putWord8 0x5B
putInstruction F32Ne =
  S.putWord8 0x5C
putInstruction F32Lt =
  S.putWord8 0x5D
putInstruction F32Gt =
  S.putWord8 0x5E
putInstruction F32Le =
  S.putWord8 0x5F
putInstruction F32Ge =
  S.putWord8 0x60
putInstruction F64Eq =
  S.putWord8 0x61
putInstruction F64Ne =
  S.putWord8 0x62
putInstruction F64Lt =
  S.putWord8 0x63
putInstruction F64Gt =
  S.putWord8 0x64
putInstruction F64Le =
  S.putWord8 0x65
putInstruction F64Ge =
  S.putWord8 0x66
putInstruction I32Clz =
  S.putWord8 0x67
putInstruction I32Ctz =
  S.putWord8 0x68
putInstruction I32Popcnt =
  S.putWord8 0x69
putInstruction I32Add =
  S.putWord8 0x6A
putInstruction I32Sub =
  S.putWord8 0x6B
putInstruction I32Mul =
  S.putWord8 0x6C
putInstruction I32DivS =
  S.putWord8 0x6D
putInstruction I32DivU =
  S.putWord8 0x6E
putInstruction I32RemS =
  S.putWord8 0x6F
putInstruction I32RemU =
  S.putWord8 0x70
putInstruction I32And =
  S.putWord8 0x71
putInstruction I32Or =
  S.putWord8 0x72
putInstruction I32Xor =
  S.putWord8 0x73
putInstruction I32Shl =
  S.putWord8 0x74
putInstruction I32ShrS =
  S.putWord8 0x75
putInstruction I32ShrU =
  S.putWord8 0x76
putInstruction I32Rotl =
  S.putWord8 0x77
putInstruction I32Rotr =
  S.putWord8 0x78
putInstruction I64Clz =
  S.putWord8 0x79
putInstruction I64Ctz =
  S.putWord8 0x7A
putInstruction I64Popcnt =
  S.putWord8 0x7B
putInstruction I64Add =
  S.putWord8 0x7C
putInstruction I64Sub =
  S.putWord8 0x7D
putInstruction I64Mul =
  S.putWord8 0x7E
putInstruction I64DivS =
  S.putWord8 0x7F
putInstruction I64DivU =
  S.putWord8 0x80
putInstruction I64RemS =
  S.putWord8 0x81
putInstruction I64RemU =
  S.putWord8 0x82
putInstruction I64And =
  S.putWord8 0x83
putInstruction I64Or =
  S.putWord8 0x84
putInstruction I64Xor =
  S.putWord8 0x85
putInstruction I64Shl =
  S.putWord8 0x86
putInstruction I64ShrS =
  S.putWord8 0x87
putInstruction I64ShrU =
  S.putWord8 0x88
putInstruction I64Rotl =
  S.putWord8 0x89
putInstruction I64Rotr =
  S.putWord8 0x8A
putInstruction F32Abs =
  S.putWord8 0x8B
putInstruction F32Neg =
  S.putWord8 0x8C
putInstruction F32Ceil =
  S.putWord8 0x8D
putInstruction F32Floor =
  S.putWord8 0x8E
putInstruction F32Trunc =
  S.putWord8 0x8F
putInstruction F32Nearest =
  S.putWord8 0x90
putInstruction F32Sqrt =
  S.putWord8 0x91
putInstruction F32Add =
  S.putWord8 0x92
putInstruction F32Sub =
  S.putWord8 0x93
putInstruction F32Mul =
  S.putWord8 0x94
putInstruction F32Div =
  S.putWord8 0x95
putInstruction F32Min =
  S.putWord8 0x96
putInstruction F32Max =
  S.putWord8 0x97
putInstruction F32Copysign =
  S.putWord8 0x98
putInstruction F64Abs =
  S.putWord8 0x99
putInstruction F64Neg =
  S.putWord8 0x9A
putInstruction F64Ceil =
  S.putWord8 0x9B
putInstruction F64Floor =
  S.putWord8 0x9C
putInstruction F64Trunc =
  S.putWord8 0x9D
putInstruction F64Nearest =
  S.putWord8 0x9E
putInstruction F64Sqrt =
  S.putWord8 0x9F
putInstruction F64Add =
  S.putWord8 0xA0
putInstruction F64Sub =
  S.putWord8 0xA1
putInstruction F64Mul =
  S.putWord8 0xA2
putInstruction F64Div =
  S.putWord8 0xA3
putInstruction F64Min =
  S.putWord8 0xA4
putInstruction F64Max =
  S.putWord8 0xA5
putInstruction F64Copysign =
  S.putWord8 0xA6
putInstruction I32WrapI64 =
  S.putWord8 0xA7
putInstruction I32TruncF32S =
  S.putWord8 0xA8
putInstruction I32TruncF32U =
  S.putWord8 0xA9
putInstruction I32TruncF64S =
  S.putWord8 0xAA
putInstruction I32TruncF64U =
  S.putWord8 0xAB
putInstruction I64ExtendI32S =
  S.putWord8 0xAC
putInstruction I64ExtendI32U =
  S.putWord8 0xAD
putInstruction I64TruncF32S =
  S.putWord8 0xAE
putInstruction I64TruncF32U =
  S.putWord8 0xAF
putInstruction I64TruncF64S =
  S.putWord8 0xB0
putInstruction I64TruncF64U =
  S.putWord8 0xB1
putInstruction F32ConvertI32S =
  S.putWord8 0xB2
putInstruction F32ConvertI32U =
  S.putWord8 0xB3
putInstruction F32ConvertI64U =
  S.putWord8 0xB4
putInstruction F32ConvertI64S =
  S.putWord8 0xB5
putInstruction F32DemoteF64 =
  S.putWord8 0xB6
putInstruction F64ConvertI32S =
  S.putWord8 0xB7
putInstruction F64ConvertI32U =
  S.putWord8 0xB8
putInstruction F64ConvertI64S =
  S.putWord8 0xB9
putInstruction F64ConvertI64U =
  S.putWord8 0xBA
putInstruction F64PromoteF32 =
  S.putWord8 0xBB
putInstruction I32ReinterpretF32 =
  S.putWord8 0xBC
putInstruction I64ReinterpretF64 =
  S.putWord8 0xBD
putInstruction F32ReinterpretI32 =
  S.putWord8 0xBE
putInstruction F64ReinterpretI64 =
  S.putWord8 0xBF
putInstruction I32Extend8S =
  S.putWord8 0xC0
putInstruction I32Extend16S =
  S.putWord8 0xC1
putInstruction I64Extend8S =
  S.putWord8 0xC2
putInstruction I64Extend16S =
  S.putWord8 0xC3
putInstruction I64Extend32S =
  S.putWord8 0xC4
putInstruction (RefNull ht) = do
  S.putWord8 0xD0
  putHeapType ht
putInstruction RefIsNull =
  S.putWord8 0xD1
putInstruction (RefFunc fi) = do
  S.putWord8 0xD2
  putFuncIndex fi
putInstruction RefEq =
  S.putWord8 0xD3
putInstruction RefAsNonNull =
  S.putWord8 0xD4
putInstruction (BrOnNull li) = do
  S.putWord8 0xD5
  putLabelIndex li
putInstruction (BrOnNonNull li) = do
  S.putWord8 0xD6
  putLabelIndex li
putInstruction (StructNew ti) = do
  S.putWord8 0xFB
  S.putWord8 0x00
  putTypeIndex ti
putInstruction (StructNewDefault ti) = do
  S.putWord8 0xFB
  S.putWord8 0x01
  putTypeIndex ti
putInstruction (StructGet ti fi) = do
  S.putWord8 0xFB
  S.putWord8 0x02
  putTypeIndex ti
  putFieldIndex fi
putInstruction (StructGetS ti fi) = do
  S.putWord8 0xFB
  S.putWord8 0x03
  putTypeIndex ti
  putFieldIndex fi
putInstruction (StructGetU ti fi) = do
  S.putWord8 0xFB
  S.putWord8 0x04
  putTypeIndex ti
  putFieldIndex fi
putInstruction (StructSet ti fi) = do
  S.putWord8 0xFB
  S.putWord8 0x05
  putTypeIndex ti
  putFieldIndex fi
putInstruction (ArrayNew ti) = do
  S.putWord8 0xFB
  S.putWord8 0x06
  putTypeIndex ti
putInstruction (ArrayNewDefault ti) = do
  S.putWord8 0xFB
  S.putWord8 0x07
  putTypeIndex ti
putInstruction (ArrayNewFixed ti n) = do
  S.putWord8 0xFB
  S.putWord8 0x08
  putTypeIndex ti
  putULEB128 n
putInstruction (ArrayGet ti) = do
  S.putWord8 0xFB
  S.putWord8 0x0B
  putTypeIndex ti
putInstruction (ArrayGetS ti) = do
  S.putWord8 0xFB
  S.putWord8 0x0C
  putTypeIndex ti
putInstruction (ArrayGetU ti) = do
  S.putWord8 0xFB
  S.putWord8 0x0D
  putTypeIndex ti
putInstruction (ArraySet ti) = do
  S.putWord8 0xFB
  S.putWord8 0x0E
  putTypeIndex ti
putInstruction ArrayLen = do
  S.putWord8 0xFB
  S.putWord8 0x0F
putInstruction (ArrayFill ti) = do
  S.putWord8 0xFB
  S.putWord8 0x10
  putTypeIndex ti
putInstruction (ArrayCopy ti1 ti2) = do
  S.putWord8 0xFB
  S.putWord8 0x11
  putTypeIndex ti1
  putTypeIndex ti2
putInstruction (RefTest (RefType n ht)) = do
  S.putWord8 0xFB
  S.putWord8 (case n of
    NotNullable ->
      0x14
    IsNullable ->
      0x15)
  putHeapType ht
putInstruction (RefCast (RefType n ht)) = do
  S.putWord8 0xFB
  S.putWord8 (case n of
    NotNullable ->
      0x16
    IsNullable ->
      0x17)
  putHeapType ht
putInstruction AnyConvertExtern = do
  S.putWord8 0xFB
  S.putWord8 0x1A
putInstruction ExternConvertAny = do
  S.putWord8 0xFB
  S.putWord8 0x1B
putInstruction RefI31 = do
  S.putWord8 0xFB
  S.putWord8 0x1C
putInstruction I31GetS = do
  S.putWord8 0xFB
  S.putWord8 0x1D
putInstruction I31GetU = do
  S.putWord8 0xFB
  S.putWord8 0x1E
putInstruction (V128Load m) = do
  S.putWord8 0xFD
  S.putWord8 0x00
  putMemArg m
putInstruction (V128Store m) = do
  S.putWord8 0xFD
  S.putWord8 0x0B
  putMemArg m
