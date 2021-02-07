module top;
    localparam A = 1;
    localparam B = A;
    if (A) begin : blk
        localparam A = 1'sbx;
        initial $display("%b", $clog2(B + 1 + 1));
    end
endmodule
