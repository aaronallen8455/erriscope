{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Erriscope.Html.SyntaxHighlighting
  ( highlightSyntax
  ) where

import           Control.Applicative
import           Data.Char (isSpace, isUpper)
import           Data.Foldable
import qualified Data.Text as T
import           Prelude hiding (span)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes as A hiding (span)
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P

import           Erriscope.Html.TokenParser (makeTokenParser)

import           Debug.Trace

type Parser = P.Parsec String ()

highlightSyntax :: T.Text -> Html
highlightSyntax inp =
  case P.runParser (P.many parseExpr <* P.eof) () "" $ T.unpack inp of
    Left err -> traceShow err toMarkup inp
    Right exprs -> exprsToHtml exprs

parseIdentifier, parseOperator, parseStringLit :: Parser String
parseNatOrFloat :: Parser (Either Integer Double)
parseCharLit :: Parser Char
parseParens, parseBrackets, parseBraces :: Parser a -> Parser a
P.TokenParser
  { P.identifier = parseIdentifier
  , P.operator = parseOperator
  , P.charLiteral = parseCharLit
  , P.stringLiteral = parseStringLit
  , P.naturalOrFloat = parseNatOrFloat
  , P.parens = parseParens
  , P.brackets = parseBrackets
  , P.braces = parseBraces
  } = makeTokenParser P.haskellDef

reservedNames, reservedOpNames :: [String]
P.LanguageDef
  { P.reservedNames = reservedNames
  , P.reservedOpNames = reservedOpNames
  } = P.haskellDef

parseNum :: Parser String
parseNum = do
  mSign <- P.optionMaybe (P.string "-")
  float <- either show show <$> parseNatOrFloat
  pure $ fold mSign <> float

data Expr
  = Ident String
  | Op String
  | CharLit Char
  | StringLit String
  | Number String
  | Parens [Expr]
  | Brackets [Expr]
  | Braces [Expr]
  | MultiLineComment String
  | LineComment String
  | ReservedName String
  | Whitespace String
  deriving Show

parseExpr :: Parser Expr
parseExpr = P.choice $ P.try <$>
  [ ReservedName <$> parseReservedName
  , Op <$> (parseReservedOp P.<|> parseOperator P.<|> parseInfixFunction)
  , Ident <$> parseIdentifier
  , CharLit <$> parseCharLit
  , StringLit <$> parseStringLit
  , Number <$> parseNum
  , Parens <$> parseParens (P.many1 parseExpr)
  , Brackets <$> parseBrackets (P.many1 parseExpr)
  , Braces <$> parseBraces (P.many1 parseExpr)
  , MultiLineComment <$> parseMultiLineComment
  , LineComment <$> parseLineComment
  , Whitespace <$> P.many1 (P.satisfy isSpace)
  ]

parseInfixFunction :: Parser String
parseInfixFunction = do
  x <- P.between (P.char '`') (P.char '`')
         $ P.many1 (P.satisfy (\x -> not (isSpace x) && x /= '`'))
  pure $ '`' : x ++ "`"

parseReservedName :: Parser String
parseReservedName =
  P.choice $ P.try . P.string <$> "()" : reservedNames

parseReservedOp :: Parser String
parseReservedOp =
  P.choice $ P.try . P.string <$> reservedOpNames

parseMultiLineComment :: Parser String
parseMultiLineComment = do
  _ <- P.string "{-"
  inMultiLine
  where
    inMultiLine =
            ([] <$ P.try (P.string "-}"))
      P.<|> liftA2 (:) P.anyChar inMultiLine

parseLineComment :: Parser String
parseLineComment = do
  _ <- P.string "--"
  inComment
  where
    inComment =
            ([] <$ P.try P.newline)
      P.<|> liftA2 (:) P.anyChar inComment

exprsToHtml :: [Expr] -> Html
exprsToHtml = foldMap exprToHtml

exprToHtml :: Expr -> Html
exprToHtml = \case
  Ident x
    | h:_ <- x, isUpper h ->
        span ! class_ "syn-uc-identifier" $ toMarkup x
    | otherwise ->
        span ! class_ "syn-lc-identifier" $ toMarkup x
  Op o -> span ! class_ "syn-operator" $ toMarkup o
  CharLit c -> span ! class_ "syn-char-lit" $ "'" <> toMarkup c <> "'"
  StringLit s -> span ! class_ "syn-string-lit" $ "\"" <> toMarkup s <> "\""
  Number n -> span ! class_ "syn-number" $ toMarkup n
  Parens xs -> do
    span ! class_ "syn-paren" $ "("
    exprsToHtml xs
    span ! class_ "syn-paren" $ ")"
  Brackets xs -> do
    span ! class_ "syn-paren" $ "["
    exprsToHtml xs
    span ! class_ "syn-paren" $ "]"
  Braces xs -> do
    span ! class_ "syn-paren" $ "{"
    exprsToHtml xs
    span ! class_ "syn-paren" $ "}"
  MultiLineComment c -> span ! class_ "syn-comment" $ "{-" <> toMarkup c <> "-}"
  LineComment c -> span ! class_ "syn-comment" $ "--" <> toMarkup c
  ReservedName n -> span ! class_ "syn-reserved-name" $ toMarkup n
  Whitespace w -> toMarkup w
