module top;
    reg [7:0] z = 0;
`define DUMP(expr, val) $display(`"expr = %b`", val);
    initial begin
        `DUMP(z, z)
        `DUMP(i.x, 8'd1)
        `DUMP(m.i.x, 8'd2)
        `DUMP(m.blk[0].x, 8'd3)
        `DUMP(m.blk[1].x, 8'd4)
    end
endmodule
