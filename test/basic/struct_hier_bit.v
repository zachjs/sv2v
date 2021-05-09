module top;
    if (1) begin : blk
        wire [3:0] s;
    end
    assign blk.s = 4'b1001;
    initial #1 $display("%b", blk.s);
endmodule
