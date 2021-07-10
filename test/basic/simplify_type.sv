module top;
    localparam _1B1 = 1'b1;
    localparam _1SB1 = 1'sb1;
    localparam [31:0] _1B1_EXT = 1'b1;
    localparam [31:0] _1SB1_EXT = 1'sb1;
    initial begin
        $display("%d", $clog2({_1B1, 1'b0}));
        $display("%d", $clog2({_1SB1, 1'b0}));
        $display("%d", $clog2({_1B1_EXT, 1'b0}));
        $display("%d", $clog2({_1SB1_EXT, 1'b0}));
    end
endmodule
