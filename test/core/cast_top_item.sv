`ifdef REF
    `define CHECK(N) (P & (1 << (Z+N+1) - 1))
`else
    `define CHECK(N) ((Z+N+1)'(P) & (1 << (Z+N+1) - 1))
`endif

`define PRINT(N) initial #P $display(`"%b bit N is set`", P);

module mod;
    parameter unsigned P = 1;
    parameter Z = 0;

    if (!Z) begin : blk1
             if (`CHECK(0)) `PRINT(0)
        else if (`CHECK(1)) `PRINT(1)
        else if (`CHECK(2)) `PRINT(2)
    end

    if (!Z) begin : blk2
        genvar i;
        if (`CHECK(3))
            `PRINT(3)
        else
            for (i = 1; i == 1 && `CHECK(4); i = i + 1)
                `PRINT(4)
    end

    if (!Z) begin : blk3
        wire signed x = 1;
        `ifdef REF
            wire [P - 1:0] tmp = x;
            wire [7:0] y = tmp;
        `else
            wire [7:0] y = $unsigned(P'(x));
        `endif
    end
    wire [7:0] x = blk3.y;
    initial #P $display("%b x = %b", P, x);
endmodule
