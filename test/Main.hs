{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Data.Int
import Data.Text (Text)
import qualified Database.Postgres.Temp as Tmp
import GHC.Generics (Generic)
import qualified Hasql.Connection as Hasql
import Hasql.Decoders (column)
import Hasql.Interpolate
import Hasql.Interpolate.Internal.TH
import qualified Hasql.Session as Hasql
import Language.Haskell.TH
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ parserTests,
      withResource (either (error . show) pure =<< Tmp.startConfig Tmp.defaultConfig) Tmp.stop executionTests
    ]

parserTests :: TestTree
parserTests =
  testGroup
    "parser"
    [ testCase "quote" testParseQuotes,
      testCase "comment" testParseComment,
      testCase "param" testParseParam
    ]

executionTests :: IO Tmp.DB -> TestTree
executionTests getDb =
  testGroup
    "execution"
    ( ($ getDb)
        <$> [ testCase "basic" . testBasic,
              testCase "composite test" . testComposite,
              testCase "row" . testRow,
              testCase "row generic" . testRowGeneric,
              testCase "snippet" . testSnippet
            ]
    )

testParseQuotes :: IO ()
testParseQuotes = do
  let expected = SqlExpr expectedSqlExpr [] [] 0
      expectedSqlExpr =
        [ Sbe'Quote "#{bonk}",
          Sbe'Sql " ",
          Sbe'Quote "^{z''onk}",
          Sbe'Sql " ",
          Sbe'Ident "#{k\"\"onk}",
          Sbe'Sql " ",
          Sbe'DollarQuote "tag" "#{kiplonk}",
          Sbe'Sql " ",
          Sbe'Cquote "newline \\n escaped \\'string\\'"
        ]
  parseSqlExpr "'#{bonk}' '^{z''onk}' \"#{k\"\"onk}\" $tag$#{kiplonk}$tag$ E'newline \\n escaped \\'string\\''" @?= Right expected

testParseComment :: IO ()
testParseComment = do
  let expected = SqlExpr expectedSqlExpr [] [] 0
      expectedSqlExpr =
        [ Sbe'Sql "content ",
          Sbe'Sql "\nhello ",
          Sbe'Sql " world\n",
          Sbe'Sql " end\n"
        ]
      inputStr =
        unlines
          [ "content -- trailing comment",
            "hello /* / comment * */ world",
            "/* comment",
            "blerg /* nested comment */",
            "*/ end"
          ]
  parseSqlExpr inputStr @?= Right expected

testParseParam :: IO ()
testParseParam = do
  let expected =
        SqlExpr
          [Sbe'Param, Sbe'Sql " ", Sbe'Param]
          [Pe'Exp (VarE (mkName "x")), Pe'Exp (LitE (IntegerL 2))]
          []
          0
  parseSqlExpr "#{x} #{2}" @?= Right expected

testBasic :: IO Tmp.DB -> IO ()
testBasic getDb = do
  withLocalTransaction getDb \conn -> do
    let relation :: [(Int64, Bool, Int64)]
        relation =
          [ (0, True, 5),
            (1, True, 6),
            (2, False, 7)
          ]
    createRes <- run conn [sql| create table hasql_interpolate_test(x int8, y boolean, z int8) |]
    createRes @?= ()
    RowsAffected insertRes <- run conn [sql| insert into hasql_interpolate_test (x,y,z) select * from ^{toTable relation} |]
    insertRes @?= 3
    selectRes <- run conn [sql| select x, y, z from hasql_interpolate_test where x > #{0 :: Int64} order by x |]
    selectRes @?= filter (\(x, _, _) -> x > 0) relation

testComposite :: IO Tmp.DB -> IO ()
testComposite getDb = do
  withLocalTransaction getDb \conn -> do
    let expected = [Point 0 0, Point 1 1]
    res <- run conn [sql| select * from (values (row(0,0)), (row(1,1)) ) as t |]
    res @?= map OneColumn expected

data T = T Int64 Bool Text deriving stock (Eq, Show)

instance DecodeRow T where
  decodeRow =
    T
      <$> column decodeField
      <*> column decodeField
      <*> column decodeField

testRow :: IO Tmp.DB -> IO ()
testRow getDb = do
  withLocalTransaction getDb \conn -> do
    let expected = [T 0 True "foo", T 1 False "bar"]
    res <- run conn [sql| select * from (values (0,true,'foo'), (1,false,'bar') ) as t |]
    res @?= expected

testRowGeneric :: IO Tmp.DB -> IO ()
testRowGeneric getDb = do
  withLocalTransaction getDb \conn -> do
    let expected = [Point 0 0, Point 1 1]
    res <- run conn [sql| select * from (values (0,0), (1,1) ) as t |]
    res @?= expected

testSnippet :: IO Tmp.DB -> IO ()
testSnippet getDb = do
  withLocalTransaction getDb \conn -> do
    let expected = [Point 0 0]
    let snippet = [sql| t.y = 0 |]
    let xVal :: Int64 = 0
    res <- run conn [sql| select * from (values (0,0), (1,1) ) as t(x,y) where t.x = #{xVal} and ^{snippet} |]
    res @?= expected

withLocalTransaction :: IO Tmp.DB -> (Hasql.Connection -> IO a) -> IO a
withLocalTransaction getDb k =
  getDb >>= \db -> bracket (either (fail . show) pure =<< Hasql.acquire (Tmp.toConnectionString db)) Hasql.release \conn -> do
    let beginTrans = do
          Hasql.run (Hasql.statement () (interp False [sql| begin |])) conn >>= \case
            Left err -> fail (show err)
            Right () -> pure ()
        rollbackTrans = do
          Hasql.run (Hasql.statement () (interp False [sql| rollback |])) conn >>= \case
            Left err -> fail (show err)
            Right () -> pure ()
    bracket beginTrans (\() -> rollbackTrans) \() -> k conn

run :: DecodeResult a => Hasql.Connection -> Sql -> IO a
run conn stmt = do
  Hasql.run (Hasql.statement () (interp False stmt)) conn >>= \case
    Left err -> assertFailure ("Hasql statement unexpectedly failed with error: " <> show err)
    Right x -> pure x

data Point = Point Int64 Int64
  deriving stock (Generic, Eq, Show)
  deriving (DecodeValue) via CompositeValue Point
  deriving anyclass (DecodeRow)
