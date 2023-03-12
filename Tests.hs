module Tests where


test_1 = "void f(){}"

test_2 = "void f(){int x = 2;}"

test_3 = "void f(int x){int x = 2;}"

test_4 = "void f(int x,string y){int x = 2;}"

--ver como por este case test a dar
test_5 = "void f(int x,string y){int x = 2;} void f(int x,string y){int x = 2;} "

test_6 = "void main(int x,string y){int x = 2;string x; x = 3; length();} void f(int x,string y){int x = 2;}"

test_7 = "int calcula(int x){while(i < 7){}}"