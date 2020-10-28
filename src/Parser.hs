{-# LANGUAGE OverloadedStrings #-}
module Parser
  ( Error (..)
  , parse
  , parseTest
  ) where


import qualified AST.Name                   as Name
import qualified AST.Source                 as Src
import           Control.Applicative        ((<|>))
import qualified Data.Char                  as Char
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import qualified Reporting.Annotation       as A
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L



-- PARSER


type Parser = M.Parsec Void T.Text


data Error
  = Parsec (M.ParseErrorBundle T.Text Void)
  deriving (Show)



-- RUN PARSER


parse :: T.Text -> Either Error Src.Program
parse input =
  case M.parse program "" input of
    Left err ->
      Left (Parsec err)

    Right value ->
      Right value


parseTest :: T.Text -> IO ()
parseTest =
  M.parseTest program



-- EXPRESSIONS


program :: Parser Src.Program
program =
  Src.Program <$> M.many (spaces *> expression <* spaces)


expression :: Parser Src.Expr
expression =
  M.choice
  [ lexeme (located boolean)
  , lexeme (located nil)
  , lexeme (located symbol)
  , lexeme (located number)
  , lexeme (located string)
  , lexeme (located (collection expression))
  ]


collection :: Parser Src.Expr -> Parser Src.Value
collection p =
  let
    items =
      spaces >> M.manyTill (spaces >> p) (M.lookAhead (C.char ')'))
  in
    Src.List <$> M.between (C.char '(') (C.char ')') items


symbol :: Parser Src.Value
symbol = do
  first <- M.oneOf symbolStartChars
  rest  <- M.takeWhileP (Just "symbol name") (`elem` symbolChars)
  pure (Src.Symbol (Name.fromText (T.cons first rest)))


number :: Parser Src.Value
number =
  M.try float <|> int


int :: Parser Src.Value
int =
  L.signed (pure ()) L.decimal
  |> fmap Src.Int
  |> fmap Src.Number


float :: Parser Src.Value
float =
  L.signed (pure ()) L.float
  |> fmap Src.Float
  |> fmap Src.Number


string :: Parser Src.Value
string =
  let
    escaped =
      C.char '\\' >> M.anySingle

    strHelp =
      M.try escaped <|> M.anySingleBut '"'
  in
    fmap Src.StrLit (T.pack <$> M.between (C.char '"') (C.char '"') (M.many strHelp))


boolean :: Parser Src.Value
boolean =
  let
    true =
      C.string "true" >> pure True

    false =
      C.string "false" >> pure False
  in
    fmap Src.Boolean (true <|> false)


nil :: Parser Src.Value
nil =
  C.string "nil" >> pure Src.Nil



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
  "/.*+!-_?$%&=<>"


symbolStartChars :: String
symbolStartChars =
  letters <> upperLetters <> miscChars


symbolChars :: String
symbolChars =
  symbolStartChars <> digits
