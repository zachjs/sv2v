// pattern: size cast width 1'sb1 is not a positive integer
module top;
    initial $display((1'sb1)'(2));
endmodule
