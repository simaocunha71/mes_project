{-# LANGUAGE DeriveDataTypeable #-}
module Ast where

import Data.Data
import Data.Generics.Zipper

-- Data type of expressions
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

-- Data type of statements
data Stat = Assign String Exp
          | Declare Type String Exp
          | DeclAssign Type String Exp
          | ITE Exp [Stat] [Stat]
          | While Exp [Stat]
          | For Exp Exp Exp [Stat]
          deriving (Show, Data)
          
-- Data type of type
data Type = Int 
          | Char 
          | String 
          | Void
          deriving (Show, Data)

-- Data type of parameter (function args in a function declaration)
data Par = Parameter Type String
                deriving (Show, Data)

-- Data type of function
data Func = FunctionDeclaration Type String [Par] [Stat]
          | FunctionCall String [String]
            deriving (Show, Data)

