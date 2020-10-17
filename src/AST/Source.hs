module AST.Source
  ( Expr
  , Value (..)
  , Number (..)
  ) where


import           AST.Name             (Name)
import qualified Data.Text            as T
import qualified Reporting.Annotation as A



-- AST


type Expr =
  A.Located Value


data Value
  = Nil
  | Number Number
  | StrLit T.Text
  | Symbol Name
  | Boolean Bool
  | List [Expr]
  deriving (Show, Eq)


data Number
  = Int Integer
  | Float Double
  deriving (Show, Eq)
