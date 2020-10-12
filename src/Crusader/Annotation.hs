module Crusader.Annotation
  ( Located (..)
  , Position (..)
  ) where



-- POSITION


data Position
  = Position
    { start :: Int
    , end   :: Int
    } deriving (Show)



-- LOCATED


data Located a
  = At Position a
  deriving (Show)
