{-# LANGUAGE DeriveDataTypeable #-}
module Ast where

import Data.Data
import Data.Generics.Zipper

-- Data type of Pro
data Program = Prog [Func]
             deriving (Show, Data)

-- Data type of function
data Func = FunctionDeclaration Type String [Par] [Stat]
            deriving (Show, Data)


-- Data type of parameter (function args in a function declaration)
data Par = Parameter Type String
                deriving (Show, Data)


-- Data type of statements
data Stat = Assign String Exp
          | Declare Type String
          | DeclAssign Type String Exp
          | ITE Exp [Stat] [Stat]
          | While Exp [Stat]
          | For [Stat] Exp [Stat] [Stat]
          | FunctionCall String [Exp] 
          | Sequence [Stat]
          deriving (Show, Data)

-- Data type of type
data Type = Int 
          | Char 
          | String 
          | Void
          deriving (Show, Data)

-- Data type of expressions
data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Const Int
         | Var String
         | Boolean Bool 
         | EqualsTo Exp Exp
         | Or Exp Exp
         | And Exp Exp
         | LessThen Exp Exp
         | MoreThen Exp Exp
         deriving (Show, Data)
         








