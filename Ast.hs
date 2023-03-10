{-# LANGUAGE DeriveDataTypeable #-}
module Ast where

import Data.Data
import Data.Generics.Zipper

data Exp = Add Exp Exp
         | Mul Exp Exp
         | Const Int
         | Var String
         deriving (Show, Data)

data Condition = EqualsTo Condition Condition
               | Or Condition Condition
               | And Condition Condition
               | LessThen Condition Condition
               | MoreThen Condition Condition
               | Exp Exp
               deriving (Show, Data)