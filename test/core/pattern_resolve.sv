package P;
    function automatic integer F;
        F = '1;
    endfunction
    typedef byte G;
endpackage

module top;
    typedef struct packed {
        integer unsigned X, Y, Z;
    } T;
    parameter T W = '{X: 3, Y: 5, Z: 7};
    localparam Z = W.X;
    localparam Y = W.Z;
    initial $display(W, W.X, W.Y, W.Z, Z, Y);

    // There is disagreement among commercial simulators on whether or not type
    // names can shadow field names. sv2v allows this shadowing.
    import P::*;
    typedef struct packed {
        byte E;
        shortint F;
        integer G;
    } U;
    U a = '{F: 1, G: 2, default: 3};
    U b = '{E: 1, G: 2, default: 3};
    U c = '{F: F(), default: 4};
    `define DUMP(v) initial $display("%b %b %b", v.E, v.F, v.G);
    `DUMP(a) `DUMP(b) `DUMP(c)
endmodule
