// pattern: size cast width 1'bx is not an integer
module top;
    initial $display((1'bx)'(2));
endmodule
