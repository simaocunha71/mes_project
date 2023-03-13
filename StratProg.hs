{-# LANGUAGE DeriveDataTypeable #-}

module StratProg where

import Ast
import Data.Data
import Data.Generics.Zipper
import PLang
import Tests


import Library.StrategicData (StrategicData)
import Library.Ztrategic

instance StrategicData Exp
instance StrategicData Stat
instance StrategicData Type
instance StrategicData Par
instance StrategicData Func
instance StrategicData a => StrategicData [a]

--Escrever aqui as funçoes de strategic programming

-- Gera a ast para o input dado
ast = langParser test_6

-- Aplica a otmização do elemento neutro das operações à AST
applyNeutralOp :: [Func] -> [Func]
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