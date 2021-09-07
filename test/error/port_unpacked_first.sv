// pattern: declarations `output x \[1:0\]` and `wire x` are incompatible due to different unpacked dimensions
module top(x);
    output x [1:0];
    wire x;
endmodule
