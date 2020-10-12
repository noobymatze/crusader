{-# LANGUAGE OverloadedStrings #-}
module Crusader.Parser
  ( parse
  , parseTest
  ) where


import           Control.Applicative           ((<|>))
import qualified Crusader.Expr                 as Expr
import qualified Crusader.Reporting.Annotation as A
import qualified Data.Char                     as Char
import qualified Data.Text                     as T
import           Data.Void                     (Void)
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as L



-- PARSER


type Parser = M.Parsec Void T.Text



-- RUN PARSER


parse :: T.Text -> Either (M.ParseErrorBundle T.Text Void) Expr.Expr
parse =
  M.parse expression ""


parseTest :: T.Text -> IO ()
parseTest =
  M.parseTest expression



-- EXPRESSIONS


expression :: Parser Expr.Expr
expression =
  M.choice
  [ lexeme (located boolean)
  , lexeme (located symbol)
  , lexeme (located number)
  , lexeme (located string)
  , lexeme (located (collection expression))
  ]


collection :: Parser Expr.Expr -> Parser Expr.Value
collection p =
  let
    items =
      spaces >> M.manyTill (spaces >> p) (M.lookAhead (C.char ')'))
  in
    Expr.List <$> M.between (C.char '(') (C.char ')') items


symbol :: Parser Expr.Value
symbol = do
  first <- M.oneOf symbolStartChars
  rest  <- M.takeWhileP (Just "symbol name") (`elem` symbolChars)
  pure (Expr.Symbol (T.cons first rest))


number :: Parser Expr.Value
number =
  M.try float <|> int


int :: Parser Expr.Value
int =
  L.signed (pure ()) L.decimal
  |> fmap Expr.IntLit


float :: Parser Expr.Value
float =
  L.signed (pure ()) L.float
  |> fmap Expr.FloatLit


string :: Parser Expr.Value
string =
  let
    escaped =
      C.char '\\' >> M.anySingle

    strHelp =
      M.try escaped <|> M.anySingleBut '"'
  in
    fmap Expr.StrLit (T.pack <$> M.between (C.char '"') (C.char '"') (M.many strHelp))


boolean :: Parser Expr.Value
boolean =
  let
    true =
      C.string "true" >> pure True

    false =
      C.string "false" >> pure False
  in
    fmap Expr.Boolean (true <|> false)



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

