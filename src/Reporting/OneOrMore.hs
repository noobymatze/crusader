module Reporting.OneOrMore
  ( OneOrMore
  , one
  , more
  ) where



-- ONE OR MORE


data OneOrMore a
  = One a
  | More (OneOrMore a) (OneOrMore a)
  deriving (Show, Eq, Ord)


-- CREATING


one :: a -> OneOrMore a
one =
  One


more :: OneOrMore a -> OneOrMore a -> OneOrMore a
more =
  More
