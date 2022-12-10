module Eval where

import Data.Function (on)
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error
import Control.Applicative

import Data.Text (Text)
import Data.Text qualified as T

import Data.Map (Map)
import Data.Map qualified as Map

import Expr
-- import Debug.Trace (traceM, trace)

-- TODO use Data.Map
type Env = [Binding]

data RuntimeError = UndefinedVariable Var | DynTypeError Text | MainNotFound
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
        _ -> runtimeError $ DynTypeError $ "Cannot apply " <> tShow fn <> " to " <> tShow e2
  where 
    liftEval :: (Value -> Value -> Either RuntimeError Value) 
             -> (Expr  -> Expr  -> Eval Value)
    liftEval op e1 e2 =
      joinError $ (liftA2 op `on` eval) e1 e2

data Type = LitT LitType | LamT Type Type | Unknown
  deriving (Show, Eq)

data LitType = IntT | StringT
  deriving (Show, Eq)

data TypeError = TypeError Expr Type Type
               | UnknownType Type Expr
  deriving (Show, Eq)

-- TODO read TAPL and come back to this
-- uniTypeCheck :: Type -> Expr -> TypeCheck ()
-- uniTypeCheck expected expr@(V v) = do
--   env <- ask
--   case lookup v env of
--     Just actual -> expect expected actual expr
--     Nothing -> throwError $ UnknownType ty expr
-- uniTypeCheck expected expr@(LitInt _) = expect expected (LitT IntT) expr
-- uniTypeCheck expected expr@(LitString _) = expect expected (LitT StringT) expr
-- uniTypeCheck expected expr@(Binop Plus l r) = do
--   expect expected (LamT (LitT IntT) (LamT (LitT IntT) (LitT IntT))) expr
--   uniTypeCheck (LitT IntT) l
--   uniTypeCheck (LitT IntT) r
-- uniTypeCheck expected expr@(Binop Minus l r) = do
--   expect expected (LamT (LitT IntT) (LamT (LitT IntT) (LitT IntT))) expr
--   uniTypeCheck (LitT IntT) l
--   uniTypeCheck (LitT IntT) r
-- -- TODO ensure we're handling shadowing correctly
-- uniTypeCheck (LamT paramT bodyT) (Lam v lamBody) = local (Map.insert v paramT) $
--   uniTypeCheck bodyT lamBody
-- uniTypeCheck expected expr@(Lam _ _) 
--   = throwError $ TypeError expr expected (LamT Unknown Unknown)
-- uniTypeCheck expected (App f x) = undefined

expect :: Type -> Type -> Expr -> TypeCheck ()
expect expected actual expr
  | expected == actual = pure ()
  | otherwise = throwError $ TypeError expr expected actual

type TypeCheck a = ReaderT TypeEnv (Either TypeError) a
type TypeEnv = Map Var Type

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
valuePlus a b = Left . DynTypeError $ "cannot add " <> tShow a <> " and " <> tShow b 

valueMinus :: (Value -> Value -> Either RuntimeError Value)
valueMinus (VI a) (VI b) = Right $ VI $ a - b
valueMinus a b = Left . DynTypeError $ "cannot subtract " <> tShow a <> " and " <> tShow b 

runtimeError :: RuntimeError -> Eval a
runtimeError = lift . Left

type Eval a = ReaderT Env (Either RuntimeError) a

-- TODO better naming
evalIt :: Expr -> Either RuntimeError Value
evalIt e = runReaderT (eval e) []

joinError :: Eval (Either RuntimeError a) -> Eval a
joinError = (lift =<<)

  
