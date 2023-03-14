{-# LANGUAGE DeriveDataTypeable #-}

module StratProg where

import Ast
import Data.Data
import Data.Generics.Zipper
import LanguageParser
import LanguageUnparser
import Tests


import Library.StrategicData (StrategicData)
import Library.Ztrategic

instance StrategicData Program
instance StrategicData Exp
instance StrategicData Stat
instance StrategicData Type
instance StrategicData Par
instance StrategicData Func
instance StrategicData a => StrategicData [a]

-- Gera a ast para o input dado
ast = parse test_6

opt = applyNeutralOp ast 


-- Aplica a otmização do elemento neutro das operações à AST
applyNeutralOp :: Program -> Program
applyNeutralOp code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTP (full_tdTP step ) codeZipper
            where step = idTP  `adhocTP` expNeutralOp
        in 
        fromZipper newCode

-- Expressa o elemento neutro das operações
expNeutralOp :: Exp -> Maybe Exp
expNeutralOp (Add e (Const 0)) = Just e
expNeutralOp (Add (Const 0) t) = Just t 
expNeutralOp (Mul e (Const 1)) = Just e
expNeutralOp (Mul (Const 1) t) = Just t
expNeutralOp e = Just e 