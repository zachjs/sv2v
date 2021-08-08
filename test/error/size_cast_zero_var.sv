// pattern: size cast width 0 is not a positive integer
module top;
    wire x = 0;
    initial $display((0)'(x));
endmodule
