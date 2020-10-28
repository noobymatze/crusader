{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Eval
  ( Error (..)
  , eval
  ) where


import qualified AST.Name             as Name
import qualified AST.Source           as Src
import           Control.Monad        ((>=>))
import qualified Control.Monad        as Monad
import qualified Data.Map             as Map
import qualified Data.Text            as T
import           Debug.Trace
import qualified Eval.Environment     as Env
import qualified Reporting.Annotation as A
import qualified Reporting.OneOrMore  as OneOrMore
import qualified Reporting.Result     as Result



-- EVAL


type Result = Result.Result [T.Text] [T.Text] Error


newtype Eval a
  = Eval
    { _runEval :: Env.Environment -> (Result a, Env.Environment)
    }


data Error
  = TypeMismatch T.Text Src.Expr
  | ArityMismatch Arity Src.Expr
  | UnknownSymbol Src.Expr
  deriving (Show)


data Arity
  = Exactly Int
  | AtLeast Int
  deriving (Show)



-- WORK WITH EVAL


throw :: OneOrMore.OneOrMore Error -> Eval a
throw v =
  Eval $ \env -> (Result.throw v, env)


access :: Eval Env.Environment
access =
  Eval $ \env -> (Result.ok env, env)


update :: (Env.Environment -> Env.Environment) -> Eval ()
update f = Eval $ \env ->
  (Result.ok (), f env)



-- EVALUATE EXPRESSIONS


eval :: Src.Program -> Result [Src.Expr]
eval (Src.Program expressions) =
  traceShow expressions $
    fst $ _runEval (traverse evalHelp expressions) Env.empty


evalHelp :: Src.Expr -> Eval Src.Expr
evalHelp expr@(A.At _ value) =
  case value of
    Src.Nil ->
      pure expr

    Src.Number _ ->
      pure expr

    Src.StrLit _ ->
      pure expr

    Src.Boolean _ ->
      pure expr

    Src.Symbol symbol -> do
      env <- access
      case Env.lookup symbol env of
        Nothing ->
          throw (OneOrMore.one (UnknownSymbol expr))

        Just foundExpr ->
          pure foundExpr

    Src.List expressions ->
      evalList expr expressions


evalList :: Src.Expr -> [Src.Expr] -> Eval Src.Expr
evalList originalExpr expressions =
  case expressions of
    [] ->
      pure originalExpr

    (A.At _ (Src.Symbol "def"):A.At pos (Src.Symbol name):expr:[]) -> do
      evaled <- evalHelp expr
      update (Env.def name evaled)
      pure (A.At pos (Src.Symbol name))

    (A.At _ (Src.Symbol "if"):condition:then_:else_:[]) -> do
      expr <- evalHelp condition
      cond <- unpackBool expr
      if cond then
        evalHelp then_
      else
        evalHelp else_

    (A.At _ (Src.Symbol func):args) ->
      apply originalExpr func args


apply :: Src.Expr -> Name.Name -> [Src.Expr] -> Eval Src.Expr
apply expr func args =
  case Map.lookup func builtins of
    Nothing ->
      throw (OneOrMore.one (UnknownSymbol expr))

    Just function ->
      function expr args



-- BUILTIN FUNCTIONS


type Primitive
  = Src.Expr -> [Src.Expr] -> Eval Src.Expr


data BuiltinFunction a
  = BuiltinFunction
      { _name    :: Name.Name
      , _arity   :: Arity
      , _unpack  :: Src.Expr -> Eval a
      , _combine :: [a] -> Src.Value
      }


builtins :: Map.Map Name.Name Primitive
builtins =
  Map.fromList
  [ builtin add
  , builtin sub
  , builtin mult
  , builtin greaterThan
  , builtin greaterOrEqual
  , builtin eq
  , builtin str
  ]


builtin :: BuiltinFunction a -> (Name.Name, Primitive)
builtin function =
  ( _name function
  , fn function
  )


add :: BuiltinFunction Src.Number
add =
  BuiltinFunction
    { _name = "+"
    , _unpack = unpackNumber
    , _combine = Src.Number . sum
    , _arity = AtLeast 0
    }


sub :: BuiltinFunction Src.Number
sub =
  BuiltinFunction
  { _name = "-"
  , _unpack = unpackNumber
  , _combine = Src.Number . foldl1 (-)
  , _arity = AtLeast 1
  }


mult :: BuiltinFunction Src.Number
mult =
  BuiltinFunction
  { _name = "*"
  , _unpack = unpackNumber
  , _combine = Src.Number . product
  , _arity = AtLeast 0
  }


greaterThan :: BuiltinFunction Src.Number
greaterThan =
  BuiltinFunction
  { _name = ">"
  , _unpack = unpackNumber
  , _arity = AtLeast 1
  , _combine = Src.Boolean . and . zipWithNext (>)
  }


greaterOrEqual :: BuiltinFunction Src.Number
greaterOrEqual =
  BuiltinFunction
  { _name = ">="
  , _unpack = unpackNumber
  , _arity = AtLeast 1
  , _combine = Src.Boolean . and . zipWithNext (>=)
  }


eq :: BuiltinFunction Src.Value
eq =
  BuiltinFunction
  { _name = "="
  , _unpack = pure . A.value
  , _arity = AtLeast 1
  , _combine = Src.Boolean . and . zipWithNext (==)
  }


fn :: BuiltinFunction a -> Primitive
fn (BuiltinFunction _ arity unpack combine) expr@(A.At pos _) args = do
  _ <- checkArity arity expr args
  evaledArgs <- Monad.mapM (evalHelp >=> unpack) args
  let result = combine evaledArgs
  pure (A.at pos result)


str :: BuiltinFunction T.Text
str =
  BuiltinFunction
  { _name = "str"
  , _unpack = unpackString
  , _arity = AtLeast 0
  , _combine = Src.StrLit . T.intercalate ""
  }


checkArity :: Arity -> Src.Expr -> [Src.Expr] -> Eval ()
checkArity arity expr args =
  case arity of
    AtLeast i ->
      if length args >= i then
        pure ()
      else
        throw (OneOrMore.one (ArityMismatch arity expr))

    Exactly i ->
      if length args == i then
        pure ()
      else
        throw (OneOrMore.one (ArityMismatch arity expr))



-- UNPACKING


unpackNumber :: Src.Expr -> Eval Src.Number
unpackNumber e@(A.At _ expr) =
  case expr of
    Src.Number number ->
      pure number

    _ ->
      throw (OneOrMore.one (TypeMismatch "number" e))


unpackString :: Src.Expr -> Eval T.Text
unpackString e@(A.At _ expr) =
  case expr of
    Src.StrLit string ->
      pure string

    _ ->
      throw (OneOrMore.one (TypeMismatch "string" e))


unpackBool :: Src.Expr -> Eval Bool
unpackBool e@(A.At _ expr) =
  case expr of
    Src.Boolean value ->
      pure value

    _ ->
      throw (OneOrMore.one (TypeMismatch "boolean" e))



-- HELPERS


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a


zipWithNext :: (a -> a -> b) -> [a] -> [b]
zipWithNext f xs =
  zipWith f xs (drop 1 xs)



-- FANCY HELPERS


instance Functor Eval where
  fmap f (Eval run) = Eval $ \env ->
    let
      (result, newEnv) =
        run env
    in
      (fmap f result, newEnv)


instance Applicative Eval where
  pure a = Eval $ \env -> (pure a, env)
  a <*> b = do
    f <- a
    v <- b
    pure (f v)


instance Monad Eval where
  Eval run >>= k = Eval $ \env ->
    let
      (result, newEnv) =
        run env

      Result.Result i w a = result >>= \value ->
        let
          Eval run' =
            k value

          (r, env') =
            run' newEnv
        in
          fmap (\v -> (v, env')) r
    in
      case a of
        Result.Err err ->
          (Result.Result i w (Result.Err err), newEnv)

        Result.Ok (v, env') ->
          (Result.Result i w (Result.Ok v), env')
