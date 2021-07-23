interface intf;
    byte x;
endinterface

module mod(intf j);
    intf i();
    assign j.x = 1;
    assign i.x = 2;
    genvar z;
    for (z = 0; z < 2; z++) begin : blk
        wire [7:0] x = $bits(j.x) - 5 + z;
    end
endmodule

module top;
    byte z = 0;
    intf i();
    mod m(i);
`define DUMP(expr) $display(`"expr = %b`", expr);
    initial begin
        `DUMP(z)
        `DUMP(i.x)
        `DUMP(m.i.x)
        `DUMP(m.blk[0].x)
        `DUMP(m.blk[1].x)
    end
endmodule
