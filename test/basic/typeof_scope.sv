`define TYPEOF(x) wire [$bits(x) - 1:0]

module top;
    genvar i;
    if (1) begin : blk
        for (i = 0; i < 3; i = i + 1) begin : prev
            localparam V = i * 2;
            localparam W = V;
            wire [W:0] x;
        end
        for (i = 0; i < 2; i = i + 1) begin : loop
            `TYPEOF(prev[i+1].x) x;
            if (1) begin : a
                localparam j = i - 3;
                if (1) begin : b
                    localparam i = j + 2;
                    `TYPEOF(prev[i+2].x) x;
                    if (1) begin : c
                        localparam j = i - 4;
                        if (1) begin : d
                            localparam i = j + 7;
                            localparam z = i - 1;
                            `TYPEOF(prev[z].x) x;
                            if (1) begin : e
                                localparam i = 0;
                                localparam j = $bits(blk.loop[i].a.b.c.d.x);
                                wire [j-1:0] y;
                            end
                        end
                    end
                end
            end
        end
    end

    `TYPEOF(blk.loop[0].x) a;
    `TYPEOF(blk.loop[1].x) b;
    `TYPEOF(blk.loop[0].a.b.x) c;
    `TYPEOF(blk.loop[1].a.b.x) d;
    `TYPEOF(blk.loop[0].a.b.c.d.x) e;
    `TYPEOF(blk.loop[1].a.b.c.d.x) f;
    `TYPEOF(blk.loop[0].a.b.c.d.e.y) g;
    `TYPEOF(blk.loop[1].a.b.c.d.e.y) h;

    `define DUMP(x) assign x = 1; initial $display(`"x: %b (%0d bits)`", x, $bits(x));
    `DUMP(a) `DUMP(b) `DUMP(c) `DUMP(d) `DUMP(e) `DUMP(f) `DUMP(g) `DUMP(h)
    `DUMP(blk.loop[0].x)
    `DUMP(blk.loop[1].x)
    `DUMP(blk.loop[0].a.b.x)
    `DUMP(blk.loop[1].a.b.x)
    `DUMP(blk.loop[0].a.b.c.d.x)
    `DUMP(blk.loop[1].a.b.c.d.x)
    `DUMP(blk.loop[0].a.b.c.d.e.y)
    `DUMP(blk.loop[1].a.b.c.d.e.y)
endmodule
