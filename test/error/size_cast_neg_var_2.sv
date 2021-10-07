// pattern: size cast width 2'sb11 is not a positive integer
// location: size_cast_neg_var_2.sv:5:13
module top;
    wire x = 0;
    initial $display((2'sb11)'(x));
endmodule
