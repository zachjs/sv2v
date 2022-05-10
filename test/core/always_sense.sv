module mod(
    input wire inp1, inp2,
    output reg out1, out2, out3, out4, out5, out6, out7, out8, out9, outA, outB
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
        input reg inp1; // ignored
        g = f(ZERO) & mod.inp1;
    endfunction

    function void u;
        output reg o;
        o = inp1;
    endfunction

    task automatic asgn;
        output reg o;
        input reg i;
        o = i;
    endtask

    always_comb
        t(out1);
    always_comb
        out2 = f(ZERO);
    always_comb
        out3 = f(ZERO) & inp1;
    always_comb
        out4 = g(ZERO);
    always_comb
        out5 = flip(inp1);
    always_comb begin
        reg x;
        x = g(ZERO);
        out6 = x;
    end
    always_comb
        u(out7);
    parameter ONE = 1;
    if (ONE)
        always_comb begin
            asgn(out8, flip(inp1));
            out9 = f(ZERO);
        end
    always_latch
        if (inp1)
            outA = f(ZERO);

    struct packed {
        logic x, y;
    } s;
    assign s = {inp1, inp2};
    function automatic h;
        input reg i; // ignored
        h = s.y;
    endfunction
    always_comb
        outB = h(ZERO);
endmodule
