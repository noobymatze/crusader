module Eval.Environment
  ( Environment
  , empty
  , lookup
  , def
  ) where


import qualified AST.Name   as Name
import qualified AST.Source as Src
import qualified Data.Map   as Map
import           Prelude    hiding (lookup)



-- ENVIRONMENT


newtype Environment
  = Environment
    { _stack :: [LocalEnv]
    } deriving (Show, Eq)


newtype LocalEnv
  = LocalEnv
    { _bindings :: Map.Map Name.Name Src.Expr
    } deriving (Show, Eq)



-- CREATING ENVIRONMENT


empty :: Environment
empty =
  Environment [LocalEnv Map.empty]



-- WORKING WITH THE ENVIRONMENT


lookup :: Name.Name -> Environment -> Maybe Src.Expr
lookup name (Environment env) =
  case env of
    [] ->
      Nothing

    (LocalEnv bindings:rest) ->
      case Map.lookup name bindings of
        Nothing ->
          lookup name (Environment rest)

        Just value ->
          Just value


def :: Name.Name -> Src.Expr -> Environment -> Environment
def name expr (Environment env) =
  case env of
    [] ->
      Environment env

    (LocalEnv bindings:rest) ->
      let
        newBindings =
          Map.insert name expr bindings
      in
        Environment (LocalEnv newBindings:rest)


