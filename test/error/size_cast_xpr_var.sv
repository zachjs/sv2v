// pattern: size cast width 'shxxxxxxxx is not an integer
// location: size_cast_xpr_var.sv:5:13
module top;
    wire x = 0;
    initial $display((1 << 'x)'(x));
endmodule
