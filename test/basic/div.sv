module top;
    localparam X = 1 / 0;
    localparam Y = 'dx;
    localparam Z = 40'dx____;
    initial $display("%b", X);
    initial $display("%b", Y);
    initial $display("%b", Z);
endmodule
