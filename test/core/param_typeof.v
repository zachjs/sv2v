module mod1;
    parameter signed [15:0] X = 0;
    parameter signed [15:0] Y = 0;
    initial $display("mod1 %0d %0d %b %b", X, Y, X, Y);
endmodule

module mod2 #(
    parameter signed [15:0] X = 0,
    parameter signed [15:0] Y = 0
);
    initial $display("mod2 %0d %0d %b %b", X, Y, X, Y);
endmodule
