module Reporting.Annotation
  ( Located (..)
  , Position (..)
  , at
  ) where



-- POSITION


data Position
  = Position
    { start :: Int
    , end   :: Int
    } deriving (Show, Eq, Ord)



-- LOCATED


data Located a
  = At Position a
  deriving (Show, Eq, Ord)



-- WORKING


at :: Position -> a -> Located a
at =
  At
