typedef struct packed {
    logic x;
    logic [3:0] y;
    logic [1:0] z;
} Struct_t;

module Unpacker(in, select, a, b, c);
    parameter WIDTH = 8;
    input Struct_t [WIDTH-1:0] in;
    input logic [$clog2(WIDTH)-1:0] select;
    output logic a;
    output logic [3:0] b;
    output logic [1:0] c;
    assign a = in[select].x;
    assign b = in[select].y;
    assign c = in[select].z;
endmodule
