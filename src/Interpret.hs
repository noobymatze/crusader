module Interpret
  ( interpret
  ) where


import qualified AST.Source           as Src
import qualified Data.Text            as T
import qualified Eval
import qualified Parser
import qualified Reporting.Annotation as A
import qualified Reporting.Error      as Error
import qualified Reporting.Result     as Result



-- INTERPRET


interpret :: T.Text -> Either Error.Error [Src.Expr]
interpret input =
  case Parser.parse input of
    Left err ->
      Left (Error.Syntax err)

    Right expr ->
      -- DISCARD INFOS AND WARNINGS FOR NOW
      case Result.toEither (Eval.eval expr) of
        Left err ->
          Left (Error.Eval err)

        Right value ->
          Right value
