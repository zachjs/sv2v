interface Interface;
    logic x;
endinterface

module Module(intfs);
    parameter LEFT = 0;
    parameter RIGHT = 0;
    Interface intfs[LEFT:RIGHT];
    logic [LEFT:RIGHT] xs;
    localparam LO = LEFT > RIGHT ? RIGHT : LEFT;
    localparam HI = LEFT > RIGHT ? LEFT : RIGHT;
    for (genvar i = LO; i <= HI; i = i + 1) begin
        // intentional shadowing of dimension constants
        localparam LEFT = 0;
        localparam RIGHT = 0;
        assign xs[i] = intfs[i].x;
    end
endmodule

module Instance();
    parameter LEFT = 0;
    parameter RIGHT = 0;

    parameter INNER_LEFT = 0;
    parameter INNER_RIGHT = 0;
    parameter INNER_OFFSET = 0;

    reg [LEFT:RIGHT] xs;

    localparam DIR = LEFT >= RIGHT ? -1 : 1;
    Interface intfs[LEFT:RIGHT]();
    generate
        genvar i;
        for (i = LEFT; i <= RIGHT; i = i + DIR)
            assign intfs[i].x = xs[i];
    endgenerate

    // intentional name collision with localparams in the module
    localparam LO = INNER_LEFT >= INNER_RIGHT ? INNER_RIGHT : INNER_LEFT;
    localparam HI = INNER_LEFT >= INNER_RIGHT ? INNER_LEFT : INNER_RIGHT;
    localparam LEN = HI - LO + 1;

    Module #(INNER_LEFT + INNER_OFFSET, INNER_RIGHT + INNER_OFFSET)
        l(intfs[INNER_LEFT:INNER_RIGHT]);
    Module #(INNER_LEFT + INNER_OFFSET, INNER_RIGHT + INNER_OFFSET)
        m(intfs[LO+:LEN]);
    Module #(INNER_LEFT + INNER_OFFSET, INNER_RIGHT + INNER_OFFSET)
        n(intfs[HI-:LEN]);
endmodule
