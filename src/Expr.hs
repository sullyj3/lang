module Expr where

import Data.Text (Text)

newtype Var = Var Text
  deriving (Show, Eq)

data Expr = Plus Expr Expr | LitInt Int | Lam Var Expr | V Var | App Expr Expr
  deriving (Show, Eq)

