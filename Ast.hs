{-# LANGUAGE DeriveDataTypeable #-}
module Ast where

import Data.Data
import Data.Generics.Zipper

data Exp = Add Exp Exp
         | Mul Exp Exp
         | Const Int
         | Var String
         deriving (Show, Data)