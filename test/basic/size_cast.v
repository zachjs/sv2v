module top;
    localparam BW = 3;
    wire [2:0] test;
    assign test = 0;
    initial #1 $display(test);
endmodule
