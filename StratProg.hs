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

-- ## Optimazations

-- Aplica a otmização do elemento neutro das operações à AST
applyOptimizations :: Program -> Program
applyOptimizations code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTP (innermost step ) codeZipper
            where step = failTP `adhocTP` expNeutralOp `adhocTP` loopImprove
        in 
        fromZipper newCode

-- Expressa o elemento neutro das operações
expNeutralOp :: Exp -> Maybe Exp
expNeutralOp (Add e (Const 0)) = Just e
expNeutralOp (Add (Const 0) t) = Just t 
expNeutralOp (Mul e (Const 1)) = Just e
expNeutralOp (Mul (Const 1) t) = Just t
expNeutralOp _ = Nothing

-- Aumenta a percepção dos loops
loopImprove :: Stat -> Maybe Stat
loopImprove (While (Const 0) st) = Just (While (Boolean False) st)
loopImprove (While (Const _) st) = Just (While (Boolean True) st)
loopImprove (For r1 (Const 0) r3 r4) = Just (For r1 (Boolean False) r3 r4)
loopImprove (For r1 (Const _) r3 r4) = Just (For r1 (Boolean True) r3 r4)
loopImprove (For [] r2 [] r4) = Just (While r2 r4)
loopImprove (For r1 r2 r3 r4) = Just (Sequence (r1 ++ [(While r2 (r3 ++ r4))]))
loopImprove _ = Nothing

-- ## Smells

-- Identifica e faz refctoring de smells
smellRefactor :: Program -> Program
smellRefactor code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTP (innermost  step ) codeZipper
            where step = failTP `adhocTP` booleanLiterals
        in 
        fromZipper newCode

-- Refactor do smell: if ( f1() ) { return True } else {return False}
booleanLiterals :: Stat -> Maybe Stat
booleanLiterals (ITE (ExpFunctionCall name args) [Return (Boolean _)] [Return (Boolean _)]) = Just (Return (ExpFunctionCall name args))
booleanLiterals _ = Nothing


-- ## Bugs

-- Agrega bugs numa lista
bugTrack :: Program -> [Int]
bugTrack code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTU (full_tdTU  step ) codeZipper
            where step = failTU `adhocTU` subZero
        in 
        newCode

subZero:: Exp -> Maybe [Int]
subZero (Div e (Const 0)) = Just [1]
subZero _ = Nothing





