// pattern: size cast width 1'bx is not an integer
// location: size_cast_x_var.sv:5:13
module top;
    wire x = 0;
    initial $display((1'bx)'(x));
endmodule
