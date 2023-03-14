module Tests where


test_1 = "void f(){}"

test_2 = "void f(){int x = 2;}"

test_3 = "void f(int x){int x = 2;}"

test_4 = "void f(int x,string y){int x = 2;}"

--ver como por este case test a dar
test_5 = " void f(int x,string y){int x = 2;}    void f(int x,string y){int x = 2;} "

test_6 = "void main(int x,string y){int x = 2*1;string x; x = 3*1; length(x,2+2);} void f(int x,string y){int x = 2 + 0;}"

test_7 = "int calcula(int x){while(i < 7){int x;}}"

test_8 = "void f(int x){for(int i = 0; i < 3; i = i + 1){}}"