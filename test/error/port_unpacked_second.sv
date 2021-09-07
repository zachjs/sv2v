// pattern: declarations `output x` and `wire x \[1:0\]` are incompatible due to different unpacked dimensions
module top(x);
    output x;
    wire x [1:0];
endmodule
