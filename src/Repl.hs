module Repl where

-- import Expr
import Parse (expr)
import Eval
import Text.Megaparsec (parse, errorBundlePretty)
import System.IO (stdout, hFlush)
import Data.Text (Text)
import qualified Data.Text.IO as T

repl :: IO ()
repl = do
  putStr "⮞ "
  hFlush stdout
  l <- T.getLine
  case parse expr "interactive" l of
    Left err -> putStrLn $ errorBundlePretty err
    Right expression -> case evalIt expression of
      Right result -> print result
      Left err -> print err
  repl
      
