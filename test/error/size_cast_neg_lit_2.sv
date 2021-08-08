// pattern: size cast width 2'sb11 is not a positive integer
module top;
    initial $display((2'sb11)'(2));
endmodule
