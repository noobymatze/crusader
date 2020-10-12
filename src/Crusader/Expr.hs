module Crusader.Expr
  ( Expr
  , Value (..)
  ) where


import qualified Crusader.Annotation as A
import qualified Data.Text           as T



-- AST


type Expr =
  A.Located Value


data Value
  = IntLit Integer
  | StrLit T.Text
  | FloatLit Double
  | Symbol T.Text
  | List [Expr]
  deriving (Show)