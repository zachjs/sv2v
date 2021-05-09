module top;
    if (1) begin : blk
        struct packed {
            logic x, y;
        } [1:0] s;
    end
    assign blk.s[0].x = 0;
    assign top.blk.s[0].y = 1;
    assign top.blk.s[1].x = 1;
    assign blk.s[1].y = 0;
    initial #1 $display("%b", blk.s);
endmodule
