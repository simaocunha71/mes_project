import Runner
import Ast
-- -> fazer parsing ap ́os o pretty printing de uma ast, produz essa mesma ast
-- -> testar se diferentes estrat ́egias (topdown, bottomup, innermost, etc) usadas na elimina ̧c ̃ao de smells s ̃ao equivalentes
-- -> testar se diferentes estrat ́egias de otimiza ̧c ̃ao de express ̃oes aritméticas são equivalentes
-- -> testar se a elimina ̧c ̃ao de smells e a optimiza ̧c ̃ao de express ̃oees aritm éticas  é comutativo

prop_PrintParse :: String -> Bool
prop_PrintParse prog = parse prog == parse (unparse (parse prog))
