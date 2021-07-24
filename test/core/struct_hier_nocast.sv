module top;
    typedef struct packed {
        int A;
        byte B;
        shortint C;
    } T;
    localparam T Q = '{ A: 1, B: 2, C: 3 };
    if (1) begin : blk
        localparam T P = Q;
        logic [P.A - 1:0] a;
        logic [P.B - 1:0] b;
        logic [P.C - 1:0] c;
    end
    initial begin
        $display("%b", $bits(blk.a));
        $display("%b", $bits(blk.b));
        $display("%b", $bits(blk.c));
    end
endmodule
