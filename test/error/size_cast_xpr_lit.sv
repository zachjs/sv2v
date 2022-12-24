// pattern: size cast width 'shxxxxxxxx is not an integer
// location: size_cast_xpr_lit.sv:4:13
module top;
    initial $display((1 << 'x)'(2));
endmodule
