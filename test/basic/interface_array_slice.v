module Module(xs);
    parameter LEFT = 0;
    parameter RIGHT = 0;
    input wire [LEFT:RIGHT] xs;
endmodule

module Instance();
    parameter LEFT = 0;
    parameter RIGHT = 0;

    parameter INNER_LEFT = 0;
    parameter INNER_RIGHT = 0;
    parameter INNER_OFFSET = 0;

    reg [LEFT:RIGHT] xs;

    localparam LO = INNER_LEFT >= INNER_RIGHT ? INNER_RIGHT : INNER_LEFT;
    localparam HI = INNER_LEFT >= INNER_RIGHT ? INNER_LEFT : INNER_RIGHT;
    localparam LEN = HI - LO + 1;

    Module #(INNER_LEFT + INNER_OFFSET, INNER_RIGHT + INNER_OFFSET)
        l(xs[INNER_LEFT:INNER_RIGHT]);
    Module #(INNER_LEFT + INNER_OFFSET, INNER_RIGHT + INNER_OFFSET)
        m(xs[LO+:LEN]);
    Module #(INNER_LEFT + INNER_OFFSET, INNER_RIGHT + INNER_OFFSET)
        n(xs[HI-:LEN]);
endmodule
