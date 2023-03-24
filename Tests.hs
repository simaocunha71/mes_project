module Tests where

test_1 = "void f(){}"

test_2 = "void f(){int x = 2; return x;}"

test_3 = "void f(int x){\nint x = 2 / 0;\n\n\nint x = 2 / 0;}"

test_4 = "void f(int x,string y){int x = 2;}"

test_5 = " void f(int x,string y){int x = 2;}    void f(int x,string y){int x = 2;} "

test_6 = "void main(int x,string y){int x = 2*1;string x; x = 3*1; length(x,2+2);} void f(int x,string y){int x = 2 + 0 + 0;}"

test_7 = "int calcula(int x){while(i < 7){int x;}}"

test_8 = "void f(int x){for(int i = 0,int x =10; i < 3; i = i + 1, x = x +1){}}"

test_9 = "int calcula(int x){while(0){int x;} for(;2;){int x;}}"

test_10 = "void function (int x, char c){int i;for(int i = 0; i < 10; i=i+1){if(i + x < 3){while(i < 7){i=i-1;}}else{for(;i < 3;){x = 4 + i;}}}c = 0;}" 