// pattern: declarations `output \[1:0\] x` and `wire x` are incompatible due to different packed dimensions
module top(x);
    output [1:0] x;
    wire x;
endmodule
