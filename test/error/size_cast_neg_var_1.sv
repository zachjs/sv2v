// pattern: size cast width 1'sb1 is not a positive integer
// location: size_cast_neg_var_1.sv:5:13
module top;
    wire x = 0;
    initial $display((1'sb1)'(x));
endmodule
