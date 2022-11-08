module Eval where

import Data.Function (on)
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Applicative

import Data.Text (Text)
import Data.Text qualified as T

import Expr
-- import Debug.Trace (traceM, trace)

-- TODO use Data.Map
type Env = [Binding]

data RuntimeError = UndefinedVariable Var | TypeError Text | MainNotFound
  deriving (Show, Eq)

data Value = VL Var Expr | VI Int | VStr Text
  deriving (Show, Eq)

runProgram :: Env -> Either RuntimeError Value
runProgram env = case lookup (Var "main") env of
  Just e -> runReaderT (eval e) env
  Nothing -> Left MainNotFound

-- >>> eval . unsafeParseExpr $ "1 + 1"
-- 2
eval :: Expr -> Eval Value
eval e = do
  -- env <- ask
  -- traceM ("eval: env is " <> show env)
  -- traceM ("evaluating " <> show e)
  -- TODO this is extremely inefficient
  e' <- betaReduce e
  case e' of
    V v -> runtimeError $ UndefinedVariable v
    LitString s -> pure $ VStr s
    LitInt i -> pure $ VI i
    Binop Plus a b -> liftEval valuePlus a b
    Binop Minus a b -> liftEval valueMinus a b
    Lam v lamBody -> pure $ VL v lamBody
    App e1 e2 -> do
      fn <- eval e1
      case fn of
        VL v lamBody -> local ((v, e2) :) $ eval lamBody
        _ -> runtimeError $ TypeError $ "Cannot apply " <> tShow fn <> " to " <> tShow e2
  where 
    liftEval :: (Value -> Value -> Either RuntimeError Value) 
             -> (Expr  -> Expr  -> Eval Value)
    liftEval op e1 e2 =
      joinError $ (liftA2 op `on` eval) e1 e2


-- counterexample:
-- (|x -> |y -> x + y) 1 2
-- Lam (Var "x") (Lam (Var "y") (Plus (V (Var "x")) (V (Var "y"))))

-- substitutes using current env
betaReduce :: Expr -> Eval Expr
betaReduce = \case
  Binop op e1 e2 -> liftA2 (Binop op) (betaReduce e1) (betaReduce e2)
  -- while beta reducing inside the body of a function, clear any existing 
  -- bindings of the variable v, to ensure proper shadowing
  Lam v e -> local (filter $ (v /=) . fst) do
    e' <- betaReduce e
    pure $ Lam v e'
  -- base case - perform the substitution
  V v -> do
    env <- ask
    case lookup v env of
      Just vVal -> pure vVal
      Nothing -> pure $ V v
  App e1 e2 -> liftA2 App (betaReduce e1) (betaReduce e2)
  e -> pure e

tShow :: Show a => a -> Text
tShow = T.pack . show

valuePlus :: (Value -> Value -> Either RuntimeError Value)
valuePlus (VI a) (VI b) = Right $ VI $ a + b
valuePlus a b = Left . TypeError $ "cannot add " <> tShow a <> " and " <> tShow b 

valueMinus :: (Value -> Value -> Either RuntimeError Value)
valueMinus (VI a) (VI b) = Right $ VI $ a - b
valueMinus a b = Left . TypeError $ "cannot subtract " <> tShow a <> " and " <> tShow b 

runtimeError :: RuntimeError -> Eval a
runtimeError = lift . Left

type Eval a = ReaderT Env (Either RuntimeError) a

-- TODO better naming
evalIt :: Expr -> Either RuntimeError Value
evalIt e = runReaderT (eval e) []

joinError :: Eval (Either RuntimeError a) -> Eval a
joinError = join . fmap lift

  
