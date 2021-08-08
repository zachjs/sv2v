// pattern: size cast width 1'bx is not an integer
module top;
    wire x = 0;
    initial $display((1'bx)'(x));
endmodule
