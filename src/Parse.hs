module Parse where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Maybe (fromJust)

import Expr
import Control.Monad (void)

type Parser = Parsec Void Text

tok :: Parser a -> Parser a
tok p = p <* hspace

parseExpr :: Text -> Maybe Expr
parseExpr = parseMaybe expr

unsafeParseExpr :: Text -> Expr
unsafeParseExpr = fromJust . parseExpr

variable :: Parser Var 
variable = Var <$> do
  T.pack <$> some lowerChar
  -- -- todo allow underscores, think deeply about allowed characters
  -- c <- lowerChar
  -- cs <- some alphaNumChar
  -- pure $ T.pack (c:cs)

parens :: Parser a -> Parser a
parens = between (tok $ char '(') (char ')')

term :: Parser Expr
term = 
  try (parens (tok expr)) <|>
  try (V <$> variable) <|>
  -- TODO think about numeric literal format
  try (LitInt <$> numLit) <|>
  try (LitString <$> strLit)

numLit :: Parser Int
numLit = L.decimal

strLit :: Parser Text
strLit = between (char '"') (char '"') (T.pack <$> many (satisfy ('"' /=)))

plus :: Parser (Expr -> Expr -> Expr)
plus = binop "+" (Binop Plus)

minus :: Parser (Expr -> Expr -> Expr)
minus = binop "-" (Binop Minus)

binop :: Text -> (Expr -> Expr -> Expr) -> Parser (Expr -> Expr -> Expr)
binop c combine = combine <$ 
  (hspace *> stringTok c)

app :: Parser (Expr -> Expr -> Expr)
app = App <$ hspace1

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
infixes = chainl applications (try plus <|> minus)

expr :: Parser Expr
expr = try lam <|> infixes
-- expr = try lam <|> (makeExprParser term [[InfixL plus], [InfixL app]])

charTok :: Char -> Parser Char
charTok = tok . char

stringTok :: Text -> Parser Text
stringTok = tok . string

lam :: Parser Expr
lam = do
  _ <- charTok '|'
  v <- tok variable
  _ <- stringTok "->"
  e <- expr
  pure $ Lam v e

binding :: Parser Binding
binding = do
  v <- tok variable
  charTok '='
  e <- expr
  void newline <|> eof
  pure (v, e)

bindings :: Parser [Binding]
bindings = sepBy binding (many newline)

parseTestFile :: Show a => FilePath -> Parser a -> IO ()
parseTestFile f p = parseTest p =<< T.readFile f
  
