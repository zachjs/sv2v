module top;
    localparam BW = 3;
    logic [2:0] test;
    assign test = BW'(0);
    initial $display(test);
endmodule
