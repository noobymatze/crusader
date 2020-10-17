module Reporting.Error
  ( Error (..)
  ) where


import qualified Parser



-- ERROR


data Error
  = Parse Parser.Error
  deriving (Show)
