`define DUMP(id) \
    begin \
        x = 1'sb1; \
        $display(`"id: access a=%b b=%b`", x.a, x.b); \
        x = '{ a: 1'sb1, b: 1'sbz }; \
        $display(`"id: literal x=%b`", x); \
    end
module top;
    parameter A = 2;
    parameter B = 3;
    struct packed {
        logic [A-1:0] a;
        logic [B-1:0] b;
    } x;
    initial `DUMP(0)
    if (1) begin : blk
        localparam A = 10;
        localparam B = 11;
        initial `DUMP(1)
    end
    initial begin
        localparam A = 10;
        localparam B = 11;
        `DUMP(2)
    end
endmodule
