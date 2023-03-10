{-# LANGUAGE DeriveDataTypeable #-}
module Ast where

import Data.Data
import Data.Generics.Zipper


data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Const Int
         | Var String
         | EqualsTo Exp Exp
         | Or Exp Exp
         | And Exp Exp
         | LessThen Exp Exp
         | MoreThen Exp Exp
         | Exp Exp
         deriving (Show, Data)

data Stat = Assign String Exp
          | Declare Type String Exp
          | DeclAssign Type String Exp
          | ITE Exp [Stat] [Stat]
          | While Exp [Stat]
          | For Exp Exp Exp [Stat]
          deriving (Show, Data)

data Type = Int 
          | Char 
          | String 
          deriving (Show, Data)

-- NOME da função | Lista de vars (argumentos) | Lista de código dentro da funçãoi
data Func = FDeclare String [String] [Stat]
            deriving (Show, Data)

