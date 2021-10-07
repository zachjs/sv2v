// pattern: size cast width 0 is not a positive integer
// location: size_cast_zero_var.sv:5:13
module top;
    wire x = 0;
    initial $display((0)'(x));
endmodule
