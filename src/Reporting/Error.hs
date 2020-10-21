module Reporting.Error
  ( Error (..)
  ) where


import qualified Eval
import qualified Parser
import qualified Reporting.OneOrMore as OneOrMore



-- ERROR


data Error
  = Syntax Parser.Error
  | Eval (OneOrMore.OneOrMore Eval.Error)
  deriving (Show)
