module Example;
    parameter type T = logic [3:0];
    T v = T'('1);
    initial #1 $display("%b", v);
endmodule
module top; Example example(); endmodule
