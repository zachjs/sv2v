module top;
    generate
        if (1) begin : i
            wire [3:0] x;
        end
        if (1) begin : blk
            wire i;
            assign i = 0;
        end
    endgenerate
    initial $display("%b %b", i.x, blk.i);
endmodule
