module mod1;
    parameter shortint X = 0;
    parameter type(X) Y = 0;
    initial $display("mod1 %0d %0d %b %b", X, Y, X, Y);
endmodule

module mod2 #(
    parameter shortint X = 0,
    parameter type(X) Y = 0
);
    initial $display("mod2 %0d %0d %b %b", X, Y, X, Y);
endmodule
