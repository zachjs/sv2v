`include "always_sense.vh"
module mod(
    input wire `INPUTS,
    output reg `OUTPUTS
);
    localparam ZERO = 0;

    task automatic t;
        output reg o;
        o = inp1;
    endtask

    function automatic flop;
        input reg i;
        flop = i;
    endfunction
    function automatic flip;
        input reg i;
        flip = flop(~i);
    endfunction

    function automatic f;
        input reg i; // ignored
        f = inp2;
    endfunction
    function automatic g;
        input reg i; // ignored
        g = f(ZERO) & inp1;
    endfunction

    task automatic u;
        output reg o;
        o = inp1;
    endtask

    task automatic asgn;
        output reg o;
        input reg i;
        o = i;
    endtask

    reg start;

    always @*
        t(out1);
    always @(inp2)
        out2 = f(ZERO);
    always @(start, inp1, inp2)
        out3 = f(ZERO) & inp1;
    always @(start, inp1, inp2)
        out4 = g(ZERO);
    always @*
        out5 = flip(inp1);
    always @(start, inp1, inp2) begin : blk
        reg x;
        x = g(ZERO);
        out6 = x;
    end
    always @(inp1)
        u(out7);
    parameter ONE = 1;
    if (ONE)
        always @(start, inp1, inp2) begin
            asgn(out8, flip(inp1));
            out9 = f(ZERO);
        end
    always @(start, inp1, inp2)
        if (inp1)
            outA = f(ZERO);

    wire [1:0] s;
    assign s = {inp1, inp2};
    function automatic h;
        input reg i; // ignored
        h = s[0];
    endfunction
    always @(s[0])
        outB = h(ZERO);
    function automatic i;
        input reg x; // ignored
        i = s[1];
    endfunction
    always @(s[1])
        asgn(outC, i(ZERO));

    initial start = 0;
endmodule
