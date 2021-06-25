package PKG;
    localparam P = 1'b1;
    typedef struct packed {
        logic f;
    } foo_t;
endpackage

module top;
    PKG::foo_t b;
    logic [1:0] a;
    assign b = '{default: PKG::P};
    assign a = '{default: PKG::P};
    initial #1 $display("%b %b", a, b);
endmodule
