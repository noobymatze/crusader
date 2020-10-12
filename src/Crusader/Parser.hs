{-# LANGUAGE OverloadedStrings #-}
module Crusader.Parser
  ( parse
  , parseTest
  ) where


import           Control.Applicative        ((<|>))
import qualified Crusader.Annotation        as A
import qualified Crusader.Expr              as C
import qualified Data.Char                  as Char
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L



-- PARSER


type Parser = M.Parsec Void T.Text



-- RUN PARSER


parse :: T.Text -> Either (M.ParseErrorBundle T.Text Void) C.Expr
parse =
  M.parse expression ""


parseTest :: T.Text -> IO ()
parseTest =
  M.parseTest expression



-- EXPRESSIONS


expression :: Parser C.Expr
expression =
  M.choice
  [ lexeme (located symbol)
  , lexeme (located number)
  , lexeme (located (collection expression))
  ]


collection :: Parser C.Expr -> Parser C.Value
collection p =
  let
    items =
      spaces >> M.manyTill (spaces >> p) (M.lookAhead (C.char ')'))
  in
    C.List <$> M.between (C.char '(') (C.char ')') items


symbol :: Parser C.Value
symbol = do
  first <- M.oneOf symbolStartChars
  rest  <- M.takeWhileP (Just "symbol name") (`elem` symbolChars)
  pure (C.Symbol (T.cons first rest))


number :: Parser C.Value
number =
  M.try float <|> int


int :: Parser C.Value
int =
  L.signed (pure ()) L.decimal
  |> fmap C.IntLit


float :: Parser C.Value
float =
  L.signed (pure ()) L.float
  |> fmap C.FloatLit



-- LOCATION AND OTHER HELPERS


located :: Parser a -> Parser (A.Located a)
located p = do
  start <- M.getOffset
  value <- p
  end   <- M.getOffset
  pure (A.At (A.Position start end) value)


lexeme :: Parser a -> Parser a
lexeme =
  L.lexeme spaces


spaces :: Parser ()
spaces =
  L.space C.space1 (L.skipLineComment ";") M.empty


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a



-- CHARS

digits :: String
digits =
  "0123456789"


letters :: String
letters =
  "abcdefghijklmnopqrstuvwxyz"


upperLetters :: String
upperLetters =
  fmap Char.toUpper letters


miscChars :: String
miscChars =
  ".*+!-_?$%&=<>"


symbolStartChars :: String
symbolStartChars =
  letters <> upperLetters <> miscChars


symbolChars :: String
symbolChars =
  symbolStartChars <> digits

