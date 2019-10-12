module top;
    localparam BW = 3;
    logic [2:0] test;
    assign test = BW'(0);
    initial #1 $display(test);
endmodule
