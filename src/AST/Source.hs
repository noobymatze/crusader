{-# LANGUAGE OverloadedStrings #-}
module AST.Source
  ( Program (..)
  , Expr
  , Value (..)
  , Number (..)
  , toString
  ) where


import           AST.Name             (Name (..))
import qualified Data.Text            as T
import qualified Reporting.Annotation as A



-- AST


newtype Program
  = Program [Namespace]
  deriving (Show)


data Namespace
  = Namespace
  { _name        :: T.Text
  , _imports     :: [Expr]
  , _expressions :: [Expr]
  } deriving (Show)


type Expr
  = A.Located Value


data Value
  = Nil
  | Number Number
  | StrLit T.Text
  | Boolean Bool
  | Symbol Name
  | List [Expr]
  | Fn [Expr] Expr
  deriving (Show)


data Number
  = Int Integer
  | Float Double
  deriving (Show, Eq, Ord)



-- WORKING WITH THE AST


toString :: Expr -> T.Text
toString (A.At _ expr) =
  case expr of
    Nil ->
      "nil"

    Number (Int i) ->
      T.pack (show i)

    Number (Float d) ->
      T.pack (show d)

    StrLit value ->
      "\"" <> value <> "\""

    Boolean True ->
      "true"

    Boolean False ->
      "false"

    Symbol (Name name) ->
      name

    List expressions ->
      "(" <> T.intercalate " " (map toString expressions) <> ")"

    Fn args body ->
      "(fn" <> "[" <> T.intercalate " " (map toString args) <> "]" <> toString body <> ")"

    -- Namespace (Name name) imports forms ->
    --   T.intercalate ""
    --   [ "(ns " <> name <> T.intercalate " " (map toString imports) <> ")"
    --   , newLine
    --   , newLine
    --   , T.intercalate newLine (map toString forms)
    --   ]


newLine :: T.Text
newLine =
  "\n"



-- FANCY HELPERS


instance Eq Value where
  Nil == Nil = True
  Number a == Number b = a == b
  StrLit a == StrLit b = a == b
  Boolean a == Boolean b = a == b
  Symbol a == Symbol b = a == b
  List a == List b = fmap A.value a == fmap A.value b
  Fn a abody == Fn b bbody = a == b && abody == bbody
  _ == _ = False


instance Num Number where
  fromInteger = Int

  negate an =
    case an of
      Int num ->
        Int (negate num)

      Float num ->
        Float (negate num)

  signum an =
    case an of
      Int num ->
        Int (signum num)

      Float num ->
        Float (signum num)

  abs a =
    case a of
      Int num ->
        Int (abs num)

      Float num ->
        Float (abs num)

  an * bn =
    case (an, bn) of
      (Int a, Int b) ->
        Int (a * b)

      (Int a, Float b) ->
        Float (fromIntegral a * b)

      (Float a, Int b) ->
        Float (a * fromIntegral b)

      (Float a, Float b) ->
        Float (a * b)

  an - bn =
    case (an, bn) of
      (Int a, Int b) ->
        Int (a - b)

      (Int a, Float b) ->
        Float (fromIntegral a - b)

      (Float a, Int b) ->
        Float (a - fromIntegral b)

      (Float a, Float b) ->
        Float (a - b)

  an + bn =
    case (an, bn) of
      (Int a, Int b) ->
        Int (a + b)

      (Int a, Float b) ->
        Float (fromIntegral a + b)

      (Float a, Int b) ->
        Float (a + fromIntegral b)

      (Float a, Float b) ->
        Float (a + b)
