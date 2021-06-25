module top;
    wire [2:0] a;
    wire [6:0] b;
    wire [17:0] c;

    generate
        if (1) begin : foo
            wire [2:0] a;
            wire [14:0] b;
            wire [17:0] c;
            wire [33:0] d;
        end
    endgenerate

`define INSPECT_SIZE(expr, size) $display(`"expr -> %0d`", size);
`define INSPECT_DATA(expr) $display(`"expr -> %b`", expr);
    initial begin
        `INSPECT_SIZE(a, 3);
        `INSPECT_SIZE(a.x, 1);
        `INSPECT_SIZE(a.y, 2);

        `INSPECT_SIZE(b, 7);
        `INSPECT_SIZE(b.x, 3);
        `INSPECT_SIZE(b.y, 4);

        `INSPECT_SIZE(c, 18);
        `INSPECT_SIZE(c.x, 5);
        `INSPECT_SIZE(c.y, 6);
        `INSPECT_SIZE(c.z, 7);
        `INSPECT_SIZE(c.z.x, 3);
        `INSPECT_SIZE(c.z.y, 4);

        `INSPECT_SIZE(foo.a, 3);
        `INSPECT_SIZE(foo.a.x, 1);
        `INSPECT_SIZE(foo.a.y, 2);

        `INSPECT_SIZE(foo.b, 15);
        `INSPECT_SIZE(foo.b.x, 7);
        `INSPECT_SIZE(foo.b.y, 8);

        `INSPECT_SIZE(foo.c, 18);
        `INSPECT_SIZE(foo.c.x, 5);
        `INSPECT_SIZE(foo.c.y, 6);
        `INSPECT_SIZE(foo.c.z, 7);
        `INSPECT_SIZE(foo.c.z.x, 3);
        `INSPECT_SIZE(foo.c.z.y, 4);

        `INSPECT_SIZE(foo.d, 34);
        `INSPECT_SIZE(foo.d.x, 9);
        `INSPECT_SIZE(foo.d.y, 10);
        `INSPECT_SIZE(foo.d.z, 15);
        `INSPECT_SIZE(foo.d.z.x, 7);
        `INSPECT_SIZE(foo.d.z.y, 8);

        `INSPECT_DATA(a);
        `INSPECT_DATA(b);
        `INSPECT_DATA(c);
        `INSPECT_DATA(foo.a);
        `INSPECT_DATA(foo.b);
        `INSPECT_DATA(foo.c);
        `INSPECT_DATA(foo.d);
    end
endmodule
