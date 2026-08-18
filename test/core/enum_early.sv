module top;
    localparam type T = enum { X = 2 };
    localparam type V = byte;
    localparam type U = enum V { Y = 2 };
    T a1;
    U a2;
    if (1) begin : blk
        localparam X = 1;
        localparam Y = 1;
        T b1;
        U b2;
        initial $display("b1 %b %b", b1, X);
        initial $display("b2 %b %b", b2, Y);
    end
    initial $display("a1 %b %b", a1, X);
    initial $display("a2 %b %b", a2, Y);
endmodule
