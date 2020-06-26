package PKG;
    typedef struct packed {
        int F;
    } S;
    function automatic S f();
        return '0;
    endfunction
endpackage

module top;
    localparam PKG::S L0 = PKG::f();
    localparam int L1 = L0.F;
    localparam int L2 = $clog2(L1);
    initial $display("%b %b %b", L0, L1, L2);
endmodule
