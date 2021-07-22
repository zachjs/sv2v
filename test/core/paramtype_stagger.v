module leaf;
    parameter T = 1;
    initial #1 $display("leaf: $bits(T)=%0d", T);
endmodule

module intermediate;
    parameter U = 1;
    parameter W = 4;
    parameter S = W;
    leaf #(S) l();
    initial #2 $display("intermediate: $bits(U)=%0d W=%0d $bits(s)=%0d",
        U, W, S);
endmodule

module top;
    parameter B = 8;
    intermediate i1();
    intermediate #(B) i2();
    initial #3 $display("top: $bits(B)=%0d", B);
endmodule
