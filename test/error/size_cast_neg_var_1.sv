// pattern: size cast width 1'sb1 is not a positive integer
module top;
    wire x = 0;
    initial $display((1'sb1)'(x));
endmodule
