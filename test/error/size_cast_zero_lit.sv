// pattern: size cast width 0 is not a positive integer
// location: size_cast_zero_lit.sv:4:13
module top;
    initial $display((0)'(2));
endmodule
