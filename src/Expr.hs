module Expr where

import Data.Text (Text)
import Data.String (IsString)

type Binding = (Var, Expr)

newtype Var = Var Text
  deriving (Show, Eq, IsString)

data Expr = Plus Expr Expr | LitInt Int | Lam Var Expr | V Var | App Expr Expr
  deriving (Show, Eq)

