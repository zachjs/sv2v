`define MAKE_PRIM(typ, size) \
    reg [size-1:0] typ``_unspecified = 1; \
    reg [size-1:0] typ``_unsigned = 1; \
    reg signed [size-1:0] typ``_signed = 1;

module top;
    wire signed x;
    wire signed [1:0] y;
    assign x = 0;
    assign y = 2;
    wire signed [1:0] z;
    assign z = x % y;
    wire [3:0] w;
    assign w = z;
    initial #1 $display("%b %b %b %b", x, y, z, w);

    `MAKE_PRIM(byte, 8)
    `MAKE_PRIM(shortint, 16)
    `MAKE_PRIM(int, 32)
    integer integer_unspecified = 1;
    reg [31:0] integer_unsigned = 1;
    integer integer_signed = 1;
    `MAKE_PRIM(longint, 64)

    `MAKE_PRIM(bit, 1)
    `MAKE_PRIM(reg, 1)
    `MAKE_PRIM(logic, 1)

    reg signed [5:0] arr;

    reg signed [11:0] s;
    initial s = 1'sb1;
    reg [5:0] t;
    initial t = 1'sb1;
endmodule
