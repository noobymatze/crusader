module Reporting.Result
  ( Result (..)
  , Answer (..)
  , ok
  , throw
  , toEither
  ) where


import qualified Reporting.OneOrMore as OneOrMore



-- RESULT


data Result info warning error value
  = Result
    { _info     :: [info]
    , _warnings :: [warning]
    , _answer   :: Answer (OneOrMore.OneOrMore error) value
    } deriving (Show)


data Answer e a
  = Ok a
  | Err e
  deriving (Show)



-- CREATING RESULTS


ok :: value -> Result i w error value
ok value =
  Result
    { _info = mempty
    , _warnings = mempty
    , _answer = Ok value
    }


throw :: OneOrMore.OneOrMore error -> Result i w error value
throw err =
  Result
    { _info = mempty
    , _warnings = mempty
    , _answer = Err err
    }



-- WORKING WITH RESULTS


toEither :: Result i w e a -> Either (OneOrMore.OneOrMore e) a
toEither result =
  case _answer result of
    Err err ->
      Left err

    Ok value ->
      Right value



-- FANCY HELPERS


instance Functor (Result i w e) where
  fmap f (Result i w answer) =
    case answer of
      Ok value ->
        Result i w (Ok (f value))

      Err err ->
        Result i w (Err err)


instance Applicative (Result i w e) where
  pure = ok
  Result i1 w1 fa <*> Result i2 w2 va =
    case (fa, va) of
      (Ok f, Ok value) ->
        Result (i1 <> i2) (w1 <> w2) (Ok (f value))

      (Ok _, Err e1) ->
        Result (i1 <> i2) (w1 <> w2) (Err e1)

      (Err e1, Ok _) ->
        Result (i1 <> i2) (w1 <> w2) (Err e1)

      (Err e1, Err e2) ->
        Result (i1 <> i2) (w1 <> w2) (Err (OneOrMore.more e1 e2))



instance Monad (Result i w e) where
  Result i1 w1 fa >>= k =
    case fa of
      Err err ->
        Result i1 w1 (Err err)

      Ok value ->
        let
          Result i2 w2 a =
            k value
        in
          Result (i1 <> i2) (w1 <> w2) a


instance Semigroup a => Semigroup (Result i w e a) where
  (Result i1 w1 a1) <> (Result i2 w2 a2) =
    case (a1, a2) of
      (Err e1, Err e2) ->
        Result (i1 <> i2) (w1 <> w2) (Err (OneOrMore.more e1 e2))

      (Err e1, Ok _) ->
        Result (i1 <> i2) (w1 <> w2) (Err e1)

      (Ok _, Err e1) ->
        Result (i1 <> i2) (w1 <> w2) (Err e1)

      (Ok v1, Ok v2) ->
        Result (i1 <> i2) (w1 <> w2) (Ok (v1 <> v2))


instance Monoid a => Monoid (Result i w e a) where
  mempty = ok mempty
