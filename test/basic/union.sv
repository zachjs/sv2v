typedef union packed {
    logic [4:0] x;
    logic [4:0] y;
} A;

typedef union packed {
    logic [4:0] x;
    logic [0:4] y;
} B;

typedef union packed {
    logic [4:0] x;
    logic [1:5] y;
} C;

typedef union packed {
    logic [4:0] x;
    struct packed {
        logic [2:0] a;
        logic [1:0] b;
    } y;
    struct packed {
        logic [1:0] a;
        logic [0:2] b;
    } z;
} D;

module wrap;

    A a;
    B b;
    C c;
    D d;

    localparam delay = 10;

    initial begin

        $monitor($time, " %b %b", a.x, a.y);
        a.x = 5'b01101; #delay;
        a.y = 5'b11101; #delay;

        $monitor($time, " %b %b", b.x, b.y);
        b.x = 5'b01101; #delay;
        b.y = 5'b11101; #delay;

        $monitor($time, " %b %b", c.x, c.y);
        c.x = 5'b01101; #delay;
        c.y = 5'b11101; #delay;

        $monitor($time, " %b %b {%b %b} %b {%b %b}", d.x, d.y, d.y.a, d.y.b,
            d.z, d.z.a, d.z.b);
        d.x = 5'b01101; #delay;
        d.y = '{ a: 3'b110, b: 2'b01 }; #delay;
        d.z = '{ b: 3'b110, a: 2'b01 }; #delay;
        d.y.a = 3'b010; #delay;
        d.y.b = 2'b10; #delay;
        d.z.a = 2'b11; #delay;
        d.z.b = 3'b101; #delay;

    end

endmodule
