module top;
    parameter CONST = 1;
    localparam WIDTH = CONST * 2;
    localparam VALUE = WIDTH'(1'sb1);
    initial $display("%b", VALUE);
    if (1) begin : blk2
        localparam WIDTH = CONST * 3;
        localparam VALUE = WIDTH'(1'sb1);
        initial $display("%b", VALUE);
    end
endmodule
