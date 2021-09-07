// pattern: declarations `output x` and `wire \[1:0\] x` are incompatible due to different packed dimensions
module top(x);
    output x;
    wire [1:0] x;
endmodule
