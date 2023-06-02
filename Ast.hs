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
          | Return Exp
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
         | Not Exp
         | Const Int
         | Var String
         | Boolean Bool 
         | EqualsTo Exp Exp
         | Or Exp Exp
         | And Exp Exp
         | LessThen Exp Exp
         | MoreThen Exp Exp
         | LessEqualThen Exp Exp
         | MoreEqualThen Exp Exp
         | ExpFunctionCall String [Exp] 
         deriving (Show, Data)

instance Eq Program where
  (Prog funcs1) == (Prog funcs2) = funcs1 == funcs2
  _ == _ = False

instance Eq Func where
  (FunctionDeclaration type1 name1 pars1 stats1) == (FunctionDeclaration type2 name2 pars2 stats2) = type1 == type2 && name1 == name2 && pars1 == pars2 && stats1 == stats2
  _ == _ = False

instance Eq Par where
  (Parameter type1 name1) == (Parameter type2 name2) = type1 == type2 && name1 == name2
  _ == _ = True

instance Eq Stat where
  (Assign s1 exp1) == (Assign s2 exp2) = s1 == s2 && exp1 == exp2
  (Declare t1 s1) == (Declare t2 s2) = t1 == t2 && s1 == s2
  (DeclAssign t1 s1 exp1) == (DeclAssign t2 s2 exp2) = t1 == t2 && s1 == s2 && exp1 == exp2
  (ITE exp1 stats1a stats1b) == (ITE exp2 stats2a stats2b) = exp1 == exp2 && stats1a == stats2a && stats1b == stats2b
  (While exp1 stats1) == (While exp2 stats2) = exp1 == exp2 && stats1 == stats2
  (For init1 cond1 update1 stats1) == (For init2 cond2 update2 stats2) = init1 == init2 && cond1 == cond2 && update1 == update2 && stats1 == stats2
  (FunctionCall s1 exps1) == (FunctionCall s2 exps2) = s1 == s2 && exps1 == exps2
  (Sequence stats1) == (Sequence stats2) = stats1 == stats2
  (Return exp1) == (Return exp2) = exp1 == exp2
  _ == _ = True

instance Eq Type where
  Int == Int = True
  Char == Char = True
  String == String = True
  Void == Void = True
  _ == _ = False

instance Eq Exp where
  (Add e1 e2) == (Add e3 e4) = e1 == e3 && e2 == e4
  (Sub e1 e2) == (Sub e3 e4) = e1 == e3 && e2 == e4
  (Mul e1 e2) == (Mul e3 e4) = e1 == e3 && e2 == e4
  (Div e1 e2) == (Div e3 e4) = e1 == e3 && e2 == e4
  (Not e1) == (Not e2) = e1 == e2
  (Const n1) == (Const n2) = n1 == n2
  (Var v1) == (Var v2) = v1 == v2
  (Boolean b1) == (Boolean b2) = b1 == b2
  (EqualsTo e1 e2) == (EqualsTo e3 e4) = e1 == e3 && e2 == e4
  (Or e1 e2) == (Or e3 e4) = e1 == e3 && e2 == e4
  (And e1 e2) == (And e3 e4) = e1 == e3 && e2 == e4
  (LessThen e1 e2) == (LessThen e3 e4) = e1 == e3 && e2 == e4
  (MoreThen e1 e2) == (MoreThen e3 e4) = e1 == e3 && e2 == e4
  (LessEqualThen e1 e2) == (LessEqualThen e3 e4) = e1 == e3 && e2 == e4
  (MoreEqualThen e1 e2) == (MoreEqualThen e3 e4) = e1 == e3 && e2 == e4
  (ExpFunctionCall f1 args1) == (ExpFunctionCall f2 args2) = f1 == f2 && args1 == args2
  _ == _ = False