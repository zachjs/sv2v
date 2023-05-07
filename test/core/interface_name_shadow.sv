interface I;
    logic [3:0] x;
endinterface
module top;
    I i();
    if (1) begin : blk
        typedef logic I;
        var I i;
        assign i = 0;
    end
    initial $display("%b %b", i.x, blk.i);
endmodule
