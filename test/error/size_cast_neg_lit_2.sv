// pattern: size cast width 2'sb11 is not a positive integer
// location: size_cast_neg_lit_2.sv:4:13
module top;
    initial $display((2'sb11)'(2));
endmodule
