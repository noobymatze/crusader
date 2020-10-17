{-# LANGUAGE OverloadedStrings #-}
module Eval
  ( eval
  , evalText
  ) where


import qualified AST.Name             as Name
import qualified AST.Source           as Src
import qualified Data.Map             as Map
import qualified Data.Text            as T
import qualified Parser
import qualified Reporting.Annotation as A
import qualified Reporting.OneOrMore  as OneOrMore
import qualified Reporting.Result     as Result



-- EVAL

type Result = Result.Result [T.Text] [T.Text] Error


data Environment
  = Environment
    { map :: Map.Map Name.Name Src.Value
    } deriving (Show)


data Error
  = TypeMismatch Type Src.Expr
  | Syntax Parser.Error
  deriving (Show)


data Type
  = Number
  | Str
  | Boolean
  deriving (Show, Eq)



-- EVAL


evalText :: T.Text -> Result Src.Expr
evalText input =
  case Parser.parse input of
    Left err ->
      Result.throw (OneOrMore.one (Syntax err))

    Right expr ->
      eval expr



eval :: Src.Expr -> Result Src.Expr
eval =
  eval_ (Environment Map.empty)


eval_ :: Environment -> Src.Expr -> Result Src.Expr
eval_ env e@(A.At position expr) =
  case expr of
    Src.Nil ->
      Result.ok e

    Src.Number _ ->
      Result.ok e

    Src.StrLit _ ->
      Result.ok e

    Src.Symbol symbol ->
      -- GET FROM ENV
      undefined

    Src.List expressions ->
      evalList env position expressions


evalList :: Environment -> A.Position -> [Src.Expr] -> Result Src.Expr
evalList env pos expressions =
  case expressions of
    [] ->
      Result.ok (A.at pos (Src.List []))

    (A.At _ (Src.Symbol func):args) ->
      apply env pos func args


apply :: Environment -> A.Position -> Name.Name -> [Src.Expr] -> Result Src.Expr
apply env pos func args =
  case func of
    "+" ->
      let
        evaled =
          args
          |> fmap (\subExpr -> eval_ env subExpr >>= unpackNumber)
          |> fmap (fmap (:[]))
          |> mconcat
      in
        fmap (A.at pos . Src.Number . foldl add (Src.Int 0)) evaled

    "-" ->
      undefined


add :: Src.Number -> Src.Number -> Src.Number
add aexpr bexpr =
  case (aexpr, bexpr) of
    (Src.Int a, Src.Int b) ->
      Src.Int (a + b)

    (Src.Float a, Src.Int b) ->
      Src.Float (a + fromIntegral b)

    (Src.Int _, Src.Float _) ->
      add bexpr aexpr

    (Src.Float a, Src.Float b) ->
      Src.Float (a + b)


unpackNumber :: Src.Expr -> Result Src.Number
unpackNumber e@(A.At pos expr) =
  case expr of
    Src.Number number ->
      Result.ok number

    _ ->
      Result.throw (OneOrMore.one (TypeMismatch Number e))



-- HELPERS


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a
