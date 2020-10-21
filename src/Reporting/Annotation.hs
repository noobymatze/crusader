module Reporting.Annotation
  ( Located (..)
  , Position (..)
  , at
  , value
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



-- WORKING WITH ANNOTATION


at :: Position -> a -> Located a
at =
  At


value :: Located a -> a
value (At _ v) =
  v
