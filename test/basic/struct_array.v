module Unpacker(in, select, a, b, c);
    parameter WIDTH = 8;
    input wire [WIDTH-1:0][6:0] in;
    input wire [$clog2(WIDTH)-1:0] select;
    output wire a;
    output wire [3:0] b;
    output wire [1:0] c;
    wire [6:0] p;
    assign p = in[select];
    assign a = p[6:6];
    assign b = p[5:2];
    assign c = p[1:0];
endmodule
