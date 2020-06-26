module top;
    localparam L0 = 0;
    localparam L1 = L0;
    localparam L2 = $clog2(L1);
    initial $display("%b %b %b", L0, L1, L2);
endmodule
