module Eval where

import Data.Function (on)
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Applicative

import Data.Text (Text)
import Data.Text qualified as T

import Expr

type Env = [Binding]

data RuntimeError = UndefinedVariable Var | TypeError Text | MainNotFound
  deriving (Show, Eq)

liftEval :: (Value -> Value -> Either RuntimeError Value) -> Expr -> Expr -> Eval Value
liftEval op e1 e2 =
  joinError $ (liftA2 op `on` eval) e1 e2

data Value = VL Var Expr | VI Int
  deriving (Show, Eq)

runProgram :: Env -> Either RuntimeError Value
runProgram env = case lookup (Var "main") env of
  Just e -> runReaderT (eval e) env
  Nothing -> Left MainNotFound

-- >>> eval . unsafeParseExpr $ "1 + 1"
-- 2
eval :: Expr -> Eval Value
eval = \case
  V v -> do
    env <- ask
    maybe (runtimeError $ UndefinedVariable v)
          eval
          (lookup v env)
  LitInt i -> pure $ VI i
  Plus a b -> liftEval valuePlus a b
  Lam v e -> pure $ VL v e
  App e1 e2 -> do
    r1 <- eval e1
    case r1 of
      VL v e -> local ((v, e2) :) $ eval e
      _ -> runtimeError $ TypeError $ "Cannot apply " <> tShow r1 <> " to " <> tShow e2

tShow :: Show a => a -> Text
tShow = T.pack . show

valuePlus :: (Value -> Value -> Either RuntimeError Value)
valuePlus (VI a) (VI b) = Right $ VI $ a + b
valuePlus a b = Left . TypeError $ "cannot add " <> (tShow a) <> " and " <> (tShow b) 

runtimeError :: RuntimeError -> Eval a
runtimeError = lift . Left

type Eval a = ReaderT Env (Either RuntimeError) a

-- TODO better naming
evalIt :: Expr -> Either RuntimeError Value
evalIt e = runReaderT (eval e) []

joinError :: Eval (Either RuntimeError a) -> Eval a
joinError = join . fmap lift

  
