module AST.Name
  ( Name (..)
  , fromText
  ) where


import           Data.String
import qualified Data.Text   as T



-- NAME


newtype Name
  = Name T.Text
  deriving (Show, Eq, Ord)



-- CREATING NAMES


fromText :: T.Text -> Name
fromText =
  Name



-- FANCY HELPERS


instance IsString Name where
  fromString = Name . fromString
