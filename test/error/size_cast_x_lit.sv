// pattern: size cast width 1'bx is not an integer
// location: size_cast_x_lit.sv:4:13
module top;
    initial $display((1'bx)'(2));
endmodule
