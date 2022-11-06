module Repl where

-- import Expr
import Parse (expr, bindings)
import Eval
import Text.Megaparsec (parse, errorBundlePretty)
import System.IO (stdout, hFlush)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T

repl :: IO ()
repl = do
  putStr "lang ⮞ "
  hFlush stdout
  l <- T.getLine
  case T.words l of
    [":run", tfilename] -> execFileInteractive $ T.unpack tfilename
    _ -> evalExprInteractive l
  repl
  where
    execFileInteractive :: FilePath -> IO ()
    execFileInteractive filename = do
      contents <- T.readFile filename
      case runProgram <$> parse bindings filename contents of
        Left e -> print e
        Right (Left e) -> print e
        Right (Right result) -> print result

    evalExprInteractive :: Text -> IO ()
    evalExprInteractive l = 
      case parse expr "interactive" l of
        Left err -> putStrLn $ errorBundlePretty err
        Right expression -> case evalIt expression of
          Right result -> print result
          Left err -> print err
