
module LanguageUnparser where

import Ast


showProgram :: Program -> String
showProgram (Prog p) = showFuncs p

showFuncs :: [Func] -> String
showFuncs [h] = (showFunc h)
showFuncs [] = ""
showFuncs (h:t) = (showFunc h) ++ " " ++ (showFuncs t)

showFunc :: Func -> String
showFunc (FunctionDeclaration t name pars stats) = (showType t) ++ " " ++ name ++ "(" ++ (showPars pars) ++ "){" ++ (showStats stats) ++ "}"


showPars :: [Par] -> String
showPars [h] = (showPar h)
showPars [] = ""
showPars (h:t) = (showPar h) ++ "," ++ (showPars t)

showPar :: Par -> String
showPar (Parameter t name) = (showType t) ++ " " ++ name


showStats :: [Stat] -> String
showStats [h] = (showStat h)
showStats [] = ""
showStats (h:t) = (showStat h) ++ " " ++ (showStats t)

showStat :: Stat -> String
showStat (Assign var exp) = var ++ " = " ++ (showExp exp) ++ ";"
showStat (Declare t var) = (showType t) ++ " " ++ var ++ ";"
showStat (DeclAssign t var exp) = (showType t) ++ " " ++ var ++ " = " ++ (showExp exp) ++ ";"
showStat (ITE exp stat1 stat2) = "if(" ++ (showExp exp) ++ "){" ++ (showStats stat1) ++ "}else{" ++  (showStats stat2) ++ "}"
showStat (While exp stat) = "while(" ++ (showExp exp) ++ "){" ++ (showStats stat) ++ "}"
showStat (FunctionCall name args) = name ++ "(" ++ (showArgs args) ++ ");"
showStat (Sequence [e]) = showStat e ++ ";"
showStat (Sequence (h:t)) = showStat h ++ ";" ++ showStat (Sequence t)

showArgs :: [Exp] -> String
showArgs [h] = (showExp h)
showArgs [] = ""
showArgs (h:t) = (showExp h) ++ "," ++ (showArgs t)

showType :: Type -> String
showType Int = "int"
showType Char = "char"
showType String = "string"
showType Void = "void"

showExps :: [Exp] -> String
showExps [h] = (showExp h)
showExps [] = ""
showExps (h:t) = (showExp h) ++ " " ++ (showExps t)


showExp :: Exp -> String
showExp (Add i1 i2) = (showExp i1) ++ "+" ++ (showExp i2)
showExp (Sub i1 i2) = (showExp i1) ++ "-" ++ (showExp i2)
showExp (Mul i1 i2) = (showExp i1) ++ "*" ++ (showExp i2)
showExp (Div i1 i2) = (showExp i1) ++ "/" ++ (showExp i2)
showExp (Const i1) =  show i1
showExp (Var i1 ) =   i1
showExp (EqualsTo i1 i2) = (showExp i1) ++ "==" ++ (showExp i2)
showExp (Or i1 i2) = (showExp i1) ++ "||" ++ (showExp i2)
showExp (And i1 i2) = (showExp i1) ++ "&&" ++ (showExp i2)
showExp (LessThen i1 i2) = (showExp i1) ++ "<" ++ (showExp i2)
showExp (MoreThen i1 i2) = (showExp i1) ++ ">" ++ (showExp i2)