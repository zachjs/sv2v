// pattern: size cast width 2'sb11 is not a positive integer
module top;
    wire x = 0;
    initial $display((2'sb11)'(x));
endmodule
