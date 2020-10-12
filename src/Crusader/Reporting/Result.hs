module Crusader.Reporting.Result
  ( Result
  , Answer (..)
  ) where



-- RESULT


data Result info warning error value
  = Result
    { _info     :: info
    , _warnings :: warning
    , _answer   :: Answer error value
    } deriving (Show)


data Answer e a
  = Ok a
  | Err e
  deriving (Show)

