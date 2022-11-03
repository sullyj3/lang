module Parse where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Maybe (fromJust)

import Expr

type Parser = Parsec Void Text

tok :: Parser a -> Parser a
tok p = p <* space

parseExpr :: Text -> Maybe Expr
parseExpr = parseMaybe expr

unsafeParseExpr :: Text -> Expr
unsafeParseExpr = fromJust . parseExpr

-- seems to be a problem with parsing spaces after variables
variable = Var <$> do
  -- todo allow underscores, think deeply about allowed characters
  T.pack <$> some lowerChar

parens :: Parser a -> Parser a
parens = between (tok $ char '(') (char ')')

term :: Parser Expr
term = 
  try (parens (tok expr)) <|>
  try (V <$> variable) <|>
  -- TODO think about numeric literal format
  try (LitInt <$> L.decimal)

numLit :: Parser Int
numLit = L.decimal

plus :: Parser (Expr -> Expr -> Expr)
plus = do 
  space
  charTok '+'
  pure Plus

app :: Parser (Expr -> Expr -> Expr)
app = App <$ space1

chainl :: Parser a -> Parser (a->a->a) -> Parser a
chainl p op = p >>= rest
   where
   rest a = try (do
               f <- op
               b <- p
               rest (f a b)
            ) <|> pure a

applications :: Parser Expr 
applications = chainl term app

infixes :: Parser Expr
infixes = chainl applications plus

expr :: Parser Expr
expr = try lam <|> infixes
-- expr = try lam <|> (makeExprParser term [[InfixL plus], [InfixL app]])

charTok :: Char -> Parser Char
charTok = tok . char

stringTok :: Text -> Parser Text
stringTok = tok . string

lam :: Parser Expr
lam = do
  charTok '|'
  v <- tok variable
  stringTok "->"
  e <- expr
  pure $ Lam v e
