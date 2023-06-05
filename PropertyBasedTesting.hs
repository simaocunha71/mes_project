module PropertyBasedTesting where
import LanguageParser
import LanguageUnparser
import StratProg
import Ast
-- -> fazer parsing ap ́os o pretty printing de uma ast, produz essa mesma ast
-- -> testar se diferentes estrat ́egias (topdown, bottomup, innermost, etc) usadas na elimina ̧c ̃ao de smells s ̃ao equivalentes
-- -> testar se diferentes estrat ́egias de otimiza ̧c ̃ao de express ̃oes aritméticas são equivalentes
-- -> testar se a elimina ̧c ̃ao de smells e a optimiza ̧c ̃ao de express ̃oees aritm éticas  é comutativo

-- Parse after printing returns the same ast
prop_PrintParse :: String -> Bool
prop_PrintParse prog =  (fst $ head $ pProgram  prog ) == (fst $ head $ pProgram (showProgram (fst $ head $ pProgram prog)))

-- Test if innermost and topdown have the same resulta eliminating smells
prop_OptInnermostTP :: String -> Bool
prop_OptInnermostTP prog = (applyOptimizations $ fst $ head $ pProgram prog ) == (applyOptimizationsTD $ fst $ head $ pProgram prog )

-- Test if smell recfactoring and expression optimization are commutative operations
prop_SmellCommutativeOpt :: String -> Bool
prop_SmellCommutativeOpt prog = (smellRefactor $ applyOptimizations $ fst $ head $ pProgram prog) == (applyOptimizations $ smellRefactor $ fst $ head $ pProgram prog)