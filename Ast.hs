{-# LANGUAGE DeriveDataTypeable #-}
module Ast where

import Data.Data
import Data.Generics.Zipper

data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | EqualsTo Exp Exp
         | Or Exp Exp
         | And Exp Exp
         | LessThen Exp Exp
         | MoreThen Exp Exp
         | Const Int
         | Var String
         deriving (Show, Data)