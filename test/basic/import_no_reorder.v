`define DUMP(x) initial $display(`"x: %0d %b`", $bits(x), x);
module top;
    wire a;
    wire [7:0] b, c;
    `DUMP(a)
    `DUMP(b)
    `DUMP(c)
endmodule
