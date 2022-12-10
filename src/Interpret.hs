--
-- Replacement for Eval.hs
--
{-# LANGUAGE DataKinds #-}
module Interpret where

import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Data.Text (Text)
import Expr (Expr (..), Var(..))
import qualified Data.Text.IO as T
import qualified Data.Text as T

type Interpret a = Eff '[Reader Env, Error RuntimeError, IOE] a

-- for now, let's say no mutual recursion, bindings can only refer to ones preceding them
type Program = [(Var, Expr)]
type Env = [(Var, Value)]

data RuntimeError = UndefinedVariable Var | DynTypeError Text | MainNotFound
  deriving (Show, Eq)

data Value = VL Var Expr
           | VI Int
           | VStr Text
           | VBuiltin Builtin
           | VUnit
  deriving (Show)

-- TODO do we really need IO here?
buildEnv :: Program -> Interpret Env
buildEnv = go []
  where
    go :: Env -> Program -> Interpret Env
    go env [] = pure env
    go env ((var, expr):rest) = do
      valEither <- liftIO $ runInterpret (interpret expr) env
      case valEither of
        Left err -> throwError err
        Right val -> go ((var, val):env) rest

runInterpret :: Interpret a -> Env -> IO (Either RuntimeError a)
runInterpret m env = runEff $ runErrorNoCallStack $ runReader env m


data Builtin = Builtin
  { builtinName :: Text
  , builtinImpl :: Value -> Interpret ()
  }

instance Show Builtin where
  show = T.unpack . builtinName


builtins :: [Builtin]
builtins = [
  Builtin "print" \v -> do
    case v of
      VStr t -> liftIO $ T.putStrLn t
      _ -> throwError $ DynTypeError $ "Cannot print " <> tShow v
  ]

tShow :: Show a => a -> Text
tShow = T.pack . show

lookupEnv :: (Reader Env :> es) => Var -> Eff es (Maybe Value)
lookupEnv name = lookup name <$> ask

getEnv :: (Reader Env :> es, Error RuntimeError :> es) => Var -> Eff es Value
getEnv var = do
  lookupEnv var >>= \case
    Just v -> pure v
    Nothing -> throwError $ UndefinedVariable var

getMain :: (Reader Env :> es, Error RuntimeError :> es) => Eff es Value
getMain = do
  mMain <- lookupEnv "main"
  case mMain of
    Just main -> pure main
    Nothing -> throwError MainNotFound

runProgram :: Interpret ()
runProgram = local withBuiltins do
  _ <- interpret (V $ Var "main")
  pure ()

withBuiltins :: Env -> Env
withBuiltins = undefined

interpret :: Expr -> Interpret Value
interpret expr = undefined
  



