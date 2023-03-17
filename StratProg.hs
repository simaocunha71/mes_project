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


-- Aplica a otmização do elemento neutro das operações à AST
applyNeutralOp :: Program -> Program
applyNeutralOp code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTP (innermost step ) codeZipper
            where step = failTP `adhocTP` expNeutralOp
        in 
        fromZipper newCode

-- Expressa o elemento neutro das operações
expNeutralOp :: Exp -> Maybe Exp
expNeutralOp (Add e (Const 0)) = Just e
expNeutralOp (Add (Const 0) t) = Just t 
expNeutralOp (Mul e (Const 1)) = Just e
expNeutralOp (Mul (Const 1) t) = Just t
expNeutralOp _ = Nothing


-- Aplica a otmização da percepção dos loops
applyLoopImprove :: Program -> Program
applyLoopImprove code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTP (innermost step ) codeZipper
            where step = failTP  `adhocTP` loopImprove
        in 
        fromZipper newCode

-- Aumenta a percepção dos loops
loopImprove :: Stat -> Maybe Stat
loopImprove (While (Exp (Const 0)) st) = Just (While (Exp (Boolean False)) st)
loopImprove (While (Exp (Const _)) st) = Just (While (Exp (Boolean True)) st)
loopImprove (For r1 (Exp (Const 0)) r3 r4) = Just (For r1 (Exp (Boolean False)) r3 r4)
loopImprove (For r1 (Exp (Const _)) r3 r4) = Just (For r1 (Exp (Boolean True)) r3 r4)
loopImprove (For [] r2 [] r4) = Just (While r2 r4)
loopImprove _ = Nothing


