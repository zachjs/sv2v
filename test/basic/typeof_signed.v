`define MAKE_PRIM(typ, base, size) \
    base [size-1:0] typ``_unspecified = 1; \
    base [size-1:0] typ``_unsigned = 1; \
    base signed [size-1:0] typ``_signed = 1;

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

    `MAKE_PRIM(byte, reg, 8)
    `MAKE_PRIM(shortint, reg, 16)
    `MAKE_PRIM(int, reg, 32)
    integer integer_unspecified = 1;
    reg [31:0] integer_unsigned = 1;
    integer integer_signed = 1;
    `MAKE_PRIM(longint, reg, 64)

    `MAKE_PRIM(bit, wire, 1)
    `MAKE_PRIM(reg, reg, 1)
    `MAKE_PRIM(logic, wire, 1)

    reg signed [5:0] arr;
endmodule
