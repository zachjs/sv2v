module top;
    wire b;
    wire [1:0] a;
    assign b = 1'b1;
    assign a = {2 {1'b1}};
    initial #1 $display("%b %b", a, b);
endmodule
