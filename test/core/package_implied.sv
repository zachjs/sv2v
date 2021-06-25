package P;
    localparam X = 10;
    localparam Y = X;
endpackage

module top;
    import P::Y;
    initial $display(Y);
endmodule
