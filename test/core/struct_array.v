module Unpacker(in, select, a, b, c);
    parameter WIDTH = 8;
    input wire [WIDTH*7-1:0] in;
    input wire [$clog2(WIDTH)-1:0] select;
    output wire a;
    output wire [3:0] b;
    output wire [1:0] c;
    assign a = in[select*7+6];
    assign b = in[select*7+5-:4];
    assign c = in[select*7+1-:2];
endmodule
