module mod1;
    parameter type T = logic;
    initial $display("%0d", $bits(T));
endmodule

module mod2;
    parameter type T = logic;
    typedef logic [$bits(T) - 1:0] A;
    typedef logic [$bits(A) - 1:0] B;
    mod1 #(A) mA();
    mod1 #(B) mB();
endmodule

module mod3;
    parameter type T = logic;
    typedef logic [$bits(T) - 1:0] A;
    typedef logic [$bits(A) - 1:0] B;
    mod2 #(A) mA();
    mod2 #(B) mB();
endmodule

module mod4;
    parameter type T = logic;
    typedef logic [$bits(T) - 1:0] A;
    typedef logic [$bits(A) - 1:0] B;
    mod3 #(A) mA();
    mod3 #(B) mB();
endmodule

module top;
    typedef struct packed { int a; } X;
    typedef struct packed { int a; X b; } Y;
    typedef logic [$bits(Y) - 1:0] A;
    typedef logic [$bits(A) - 1:0] B;
    mod4 #(A) m4A();
    mod4 #(B) m4B();
endmodule
