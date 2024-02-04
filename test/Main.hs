module Main where

import System.FilePath ((</>))
import qualified GHC.Stack as GHC
import Control.Monad.IO.Class (MonadIO)
import Language.Wasm.Encoder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C
import qualified Data.Serialize as S
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)

main :: IO ()
main =
  defaultMain (testGroup "golden" [
    testEncode "example.wasm" example
  ])

golden
  :: String
  -> LBS.ByteString
  -> TestTree
golden n e =
  goldenVsString n ("test" </> "golden" </> n) (pure e)

testEncode
  :: String
  -> [Section]
  -> TestTree
testEncode n m =
  golden n (LBS.fromStrict (S.runPut (putModule m)))

example
 :: [Section]
example =
  [
    TypeSection [
      finalNoRec (StructType [
        FieldType MutConst (Val I32)
      , FieldType MutConst (Val (Ref (RefType IsNullable list)))
      ])
    , finalNoRec (FuncType (ResultType [Ref (RefType NotNullable list)]) (ResultType [I32]))
    , finalNoRec (FuncType (ResultType []) (ResultType [Ref (RefType NotNullable list)]))
    , finalNoRec (FuncType (ResultType [Ref (RefType NotNullable list), I32]) (ResultType [I32]))
    , finalNoRec (FuncType (ResultType [anyref]) (ResultType [anyref]))
    , finalNoRec (FuncType (ResultType []) (ResultType [I32]))
    ]
  , FunctionSection [
      TypeIndex 2
    , TypeIndex 1
    , TypeIndex 3
    , TypeIndex 1
    , TypeIndex 4
    , TypeIndex 5
    ]
  , ExportSection [
      Export (C.pack "main") (ExportFuncIndex (FuncIndex 5))
    ]
  , CodeSection [
      Function [] [
        I32Const 10
      , I32Const 11
      , RefNull None
      , StructNew (TypeIndex 0)
      , StructNew (TypeIndex 0)
      ]
    , Function [] [
        LocalGet (LocalIndex 0)
      , StructGet (TypeIndex 0) (FieldIndex 0)
      ]
    , Function [] [
        LocalGet (LocalIndex 1)
      , LocalGet (LocalIndex 0)
      , StructGet (TypeIndex 0) (FieldIndex 0)
      , I32Add
      , LocalSet (LocalIndex 1)

      , Block EmptyBlockType
      , LocalGet (LocalIndex 0)
      , StructGet (TypeIndex 0) (FieldIndex 1)
      , BrOnNull (LabelIndex 0)
      , LocalGet (LocalIndex 1)
      , ReturnCall (FuncIndex 2)
      , End

      , LocalGet (LocalIndex 1)
      ]
    , Function [] [
        LocalGet (LocalIndex 0)
      , I32Const 0
      , ReturnCall (FuncIndex 2)
      ]
    , Function [] [
        LocalGet (LocalIndex 0)
      ]
    , Function [] [
        Call (FuncIndex 0)
      , Call (FuncIndex 4)
      , RefCast (RefType NotNullable (Concrete 0))
      , Call (FuncIndex 3)
      ]
    ]
  ]
  where
    list =
      Concrete 0
