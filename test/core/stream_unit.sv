module top;
    parameter W = 8;
    logic [W - 1:0] x, y;
    assign y = {<<{x}};
    initial x = 8'b1101_0100;
endmodule
