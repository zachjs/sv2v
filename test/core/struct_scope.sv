module top;
    typedef struct packed {
        logic x;
        logic [1:0] y;
    } A;
    typedef struct packed {
        logic [2:0] x;
        logic [3:0] y;
    } B;
    typedef struct packed {
        logic [4:0] x;
        logic [5:0] y;
        B z;
    } C;

    A a;
    B b;
    C c;

    generate
        begin : foo
            typedef struct packed {
                logic [6:0] x;
                logic [7:0] y;
            } B;
            typedef struct packed {
                logic [8:0] x;
                logic [9:0] y;
                B z;
            } D;

            A a;
            B b;
            C c;
            D d;
        end
    endgenerate

`define INSPECT_SIZE(expr) $display(`"expr -> %0d`", $bits(expr));
`define INSPECT_DATA(expr) $display(`"expr -> %b`", expr);
    initial begin
        `INSPECT_SIZE(a);
        `INSPECT_SIZE(a.x);
        `INSPECT_SIZE(a.y);

        `INSPECT_SIZE(b);
        `INSPECT_SIZE(b.x);
        `INSPECT_SIZE(b.y);

        `INSPECT_SIZE(c);
        `INSPECT_SIZE(c.x);
        `INSPECT_SIZE(c.y);
        `INSPECT_SIZE(c.z);
        `INSPECT_SIZE(c.z.x);
        `INSPECT_SIZE(c.z.y);

        `INSPECT_SIZE(foo.a);
        `INSPECT_SIZE(foo.a.x);
        `INSPECT_SIZE(foo.a.y);

        `INSPECT_SIZE(foo.b);
        `INSPECT_SIZE(foo.b.x);
        `INSPECT_SIZE(foo.b.y);

        `INSPECT_SIZE(foo.c);
        `INSPECT_SIZE(foo.c.x);
        `INSPECT_SIZE(foo.c.y);
        `INSPECT_SIZE(foo.c.z);
        `INSPECT_SIZE(foo.c.z.x);
        `INSPECT_SIZE(foo.c.z.y);

        `INSPECT_SIZE(foo.d);
        `INSPECT_SIZE(foo.d.x);
        `INSPECT_SIZE(foo.d.y);
        `INSPECT_SIZE(foo.d.z);
        `INSPECT_SIZE(foo.d.z.x);
        `INSPECT_SIZE(foo.d.z.y);

        `INSPECT_DATA(a);
        `INSPECT_DATA(b);
        `INSPECT_DATA(c);
        `INSPECT_DATA(foo.a);
        `INSPECT_DATA(foo.b);
        `INSPECT_DATA(foo.c);
        `INSPECT_DATA(foo.d);
    end
endmodule
