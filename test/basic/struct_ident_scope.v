`define DUMP(id) \
    begin \
        x = 1'sb1; \
        $display(`"id: access a=%b b=%b`", x[0+:A], x[A+:B]); \
        x = { {A {1'sb1}}, {B {1'sbz}} }; \
        $display(`"id: literal x=%b`", x); \
    end
module top;
    parameter A = 2;
    parameter B = 3;
    reg [A+B-1:0] x;
    initial `DUMP(0)
    if (1) begin : blk
        localparam _A = 10;
        localparam _B = 11;
        initial `DUMP(1)
    end
    initial begin : foo
        localparam _A = 10;
        localparam _B = 11;
        `DUMP(2)
    end
endmodule
