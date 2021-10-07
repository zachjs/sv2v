// pattern: size cast width 1'sb1 is not a positive integer
// location: size_cast_neg_lit_1.sv:4:13
module top;
    initial $display((1'sb1)'(2));
endmodule
