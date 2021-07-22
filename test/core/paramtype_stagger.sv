module leaf;
    parameter type T = logic;
    initial #1 $display("leaf: $bits(T)=%0d", $bits(T));
endmodule

module intermediate;
    parameter type U = logic;
    parameter W = 4;
    parameter type S = logic [W - 1:0];
    leaf #(S) l();
    initial #2 $display("intermediate: $bits(U)=%0d W=%0d $bits(s)=%0d",
        $bits(U), W, $bits(S));
endmodule

module top;
    parameter type B = byte;
    intermediate i1();
    intermediate #(B) i2();
    initial #3 $display("top: $bits(B)=%0d", $bits(B));
endmodule
