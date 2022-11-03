module Expr where

import Data.Text (Text)

type Binding = (Var, Expr)

newtype Var = Var Text
  deriving (Show, Eq)

data Expr = Plus Expr Expr | LitInt Int | Lam Var Expr | V Var | App Expr Expr
  deriving (Show, Eq)

