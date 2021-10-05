// pattern: assert_deferred_nonzero\.sv:3:21: Parse error: expected 0 after #, but found 1
module top;
    initial assert #1 (1);
endmodule
