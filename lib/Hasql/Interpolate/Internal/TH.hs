{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hasql.Interpolate.Internal.TH
  ( sql,
    addParam,
    parseSqlExpr,
    compileSqlExpr,
    SqlExpr (..),
    SqlBuilderExp (..),
    ParamEncoder (..),
    SpliceBind (..),
  )
where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Array (listArray, (!))
import Data.ByteString.Builder (Builder, stringUtf8)
import Data.Char
import Data.Functor
import Data.Functor.Contravariant
import qualified Data.IntSet as IS
import Data.Monoid (Ap (..))
import Data.Void
import qualified Hasql.Encoders as E
import Hasql.Interpolate.Internal.Encoder (EncodeField (..))
import Hasql.Interpolate.Internal.Sql
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    anySingle,
    chunk,
    eof,
    notFollowedBy,
    runParser,
    single,
    takeWhileP,
    try,
  )

data SqlExpr = SqlExpr
  { sqlBuilderExp :: [SqlBuilderExp],
    paramEncoder :: [ParamEncoder],
    spliceBinds :: [SpliceBind],
    bindCount :: Int
  }
  deriving stock (Show, Eq)

data SqlBuilderExp
  = Sbe'Var Int
  | Sbe'Param
  | Sbe'Quote String
  | Sbe'Ident String
  | Sbe'DollarQuote String String
  | Sbe'Cquote String
  | Sbe'Sql String
  deriving stock (Show, Eq)

data ParamEncoder
  = Pe'Exp Exp
  | Pe'Var Int
  deriving stock (Show, Eq)

data SpliceBind = SpliceBind
  { sbBuilder :: Int,
    sbParamEncoder :: Int,
    sbExp :: Exp
  }
  deriving stock (Show, Eq)

dollar :: Builder
dollar = "$"

cquote :: Builder
cquote = "E'"

sq :: Builder
sq = "'"

dq :: Builder
dq = "\""

data ParserState = ParserState
  { ps'sqlBuilderExp :: [SqlBuilderExp] -> [SqlBuilderExp],
    ps'paramEncoder :: [ParamEncoder] -> [ParamEncoder],
    ps'spliceBinds :: [SpliceBind] -> [SpliceBind],
    ps'nextUnique :: Int
  }

type Parser a = StateT (ParserState) (Parsec Void String) a

sqlExprParser :: Parser ()
sqlExprParser = go
  where
    go =
      quoted
        <|> ident
        <|> dollarQuotes
        <|> cquoted
        <|> param
        <|> splice
        <|> comment
        <|> multilineComment
        <|> someSql
        <|> eof

    nextUnique :: Parser Int
    nextUnique = do
      st <- get
      let next = ps'nextUnique st
          !nextnext = next + 1
      put st {ps'nextUnique = nextnext}
      pure next

    appendSqlBuilderExp :: SqlBuilderExp -> Parser ()
    appendSqlBuilderExp x = do
      st <- get
      put st {ps'sqlBuilderExp = ps'sqlBuilderExp st . (x :)}

    appendEncoder :: ParamEncoder -> Parser ()
    appendEncoder x = do
      st <- get
      put st {ps'paramEncoder = ps'paramEncoder st . (x :)}

    addSpliceBinding :: Exp -> Parser ()
    addSpliceBinding x = do
      exprVar <- nextUnique
      paramVar <- nextUnique
      st <- get
      put
        st
          { ps'spliceBinds =
              ps'spliceBinds st
                . (SpliceBind {sbBuilder = exprVar, sbParamEncoder = paramVar, sbExp = x} :)
          }
      appendSqlBuilderExp (Sbe'Var exprVar)
      appendEncoder (Pe'Var paramVar)

    comment = do
      _ <- chunk "--"
      void $ takeWhileP (Just "comment") (/= '\n')
      go

    multilineComment = do
      multilineCommentBegin
      go

    multilineCommentBegin = do
      _ <- chunk "/*"
      multilineCommentEnd

    multilineCommentEnd = do
      void $ takeWhileP (Just "multiline comment") (\c -> c /= '*' && c /= '/')
      (multilineCommentBegin >> multilineCommentEnd) <|> void (chunk "*/") <|> (anySingle >> multilineCommentEnd)

    escapedContent name terminal escapeChar escapeParser =
      let loop sofar = do
            content <- takeWhileP (Just name) (\c -> c /= terminal && c /= escapeChar)
            notFollowedBy eof
            (try escapeParser >>= \esc -> loop (sofar . (content ++) . (esc ++)))
              <|> (single terminal $> sofar content)
       in loop id

    betwixt name initial terminal escapeChar escapeParser = do
      _ <- chunk initial
      escapedContent name terminal escapeChar escapeParser

    quoted = do
      content <- betwixt "single quotes" "'" '\'' '\'' (chunk "''")
      appendSqlBuilderExp (Sbe'Quote content)
      go

    cquoted = do
      content <- betwixt "C-style escape quote" "E'" '\'' '\\' do
        a <- single '\\'
        b <- anySingle
        pure [a, b]
      appendSqlBuilderExp (Sbe'Cquote content)
      go

    ident = do
      content <- betwixt "identifier" "\"" '"' '"' (chunk "\"\"")
      appendSqlBuilderExp (Sbe'Ident content)
      go

    dollarQuotes = do
      _ <- single '$'
      tag <- takeWhileP (Just "identifier") isAlphaNum
      _ <- single '$'
      let bonk sofar = do
            notFollowedBy eof
            c <- takeWhileP (Just "dollar quoted content") (/= '$')
            (parseEndQuote $> (sofar . (c ++))) <|> bonk (sofar . (c ++))
          parseEndQuote = do
            _ <- single '$'
            _ <- chunk tag
            void $ single '$'
      content <- ($ "") <$> bonk id
      appendSqlBuilderExp (Sbe'DollarQuote tag content)
      go

    param = do
      _ <- chunk "#{"
      content <- takeWhileP (Just "parameter") (/= '}')
      _ <- single '}'
      alpha <-
        case parseExp content of
          Left err -> fail err
          Right x -> pure x
      appendEncoder (Pe'Exp alpha)
      appendSqlBuilderExp Sbe'Param
      go

    splice = do
      _ <- chunk "^{"
      content <- takeWhileP (Just "splice") (/= '}')
      _ <- single '}'
      alpha <-
        case parseExp content of
          Left err -> fail err
          Right x -> pure x
      addSpliceBinding alpha
      go

    breakCharsIS = IS.fromList (map fromEnum breakChars)
    breakChars =
      [ '\'',
        'E',
        '"',
        '#',
        '^',
        '$',
        '-',
        '/'
      ]

    someSql = do
      s <- anySingle
      content <- takeWhileP (Just "sql") (\c -> IS.notMember (fromEnum c) breakCharsIS)
      appendSqlBuilderExp (Sbe'Sql (s : content))
      go

addParam :: State Int Builder
addParam = state \i ->
  let !i' = i + 1
   in (dollar <> stringUtf8 (show i), i')

parseSqlExpr :: String -> Either (ParseErrorBundle String Void) SqlExpr
parseSqlExpr str = do
  ps <- runParser (execStateT sqlExprParser (ParserState id id id 0)) "" str
  pure
    SqlExpr
      { sqlBuilderExp = ps'sqlBuilderExp ps [],
        paramEncoder = ps'paramEncoder ps [],
        spliceBinds = ps'spliceBinds ps [],
        bindCount = ps'nextUnique ps
      }

-- | QuasiQuoter that supports interpolation and splices. Produces a
-- 'Sql'.
--
-- @#{..}@ interpolates a haskell expression into a sql query.
--
-- @
-- example1 :: EncodeValue a => a -> Sql
-- example1 x = [sql| select \#{x} |]
-- @
--
-- @^{..}@ introduces a splice, which allows us to inject a sql
-- snippet along with the associated parameters into another sql
-- snippet.
--
-- @
-- example2 :: Sql
-- example2 = [sql| ^{example1 True} where true |]
-- @
sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = \str -> do
        case parseSqlExpr str of
          Left err -> fail (show err)
          Right sqlExpr -> compileSqlExpr sqlExpr,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

compileSqlExpr :: SqlExpr -> Q Exp
compileSqlExpr (SqlExpr sqlBuilder enc spliceBindings bindCount) = do
  nameArr <- listArray (0, bindCount - 1) <$> replicateM bindCount (newName "x")
  let spliceDecs =
        map
          ( \SpliceBind {sbBuilder, sbParamEncoder, sbExp} ->
              ValD (ConP 'Sql (map VarP [nameArr ! sbBuilder, nameArr ! sbParamEncoder])) (NormalB sbExp) []
          )
          spliceBindings
  sqlBuilderExp <-
    let go a b = case a of
          Sbe'Var i -> [e|Ap $(varE (nameArr ! i)) <> $b|]
          Sbe'Param -> [e|Ap addParam <> $b|]
          Sbe'Quote content -> [e|pure (sq <> stringUtf8 content <> sq) <> $b|]
          Sbe'Ident content -> [e|pure (dq <> stringUtf8 content <> dq) <> $b|]
          Sbe'DollarQuote tag content -> [e|pure (dollar <> stringUtf8 tag <> dollar <> stringUtf8 content <> dollar <> stringUtf8 tag <> dollar) <> $b|]
          Sbe'Cquote content -> [e|pure (cquote <> content <> sq) <> $b|]
          Sbe'Sql content -> [e|pure (stringUtf8 content) <> $b|]
     in foldr go [e|pure mempty|] sqlBuilder
  encExp <-
    let go a b = case a of
          Pe'Exp x -> [e|$(pure x) >$ E.param encodeField <> $b|]
          Pe'Var x -> [e|$(varE (nameArr ! x)) <> $b|]
     in foldr go [e|mempty|] enc
  body <- [e|Sql (getAp $(pure sqlBuilderExp)) $(pure encExp)|]
  pure case spliceDecs of
    [] -> body
    _ -> LetE spliceDecs body
