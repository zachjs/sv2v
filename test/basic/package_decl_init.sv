package PKG;
    function automatic logic f;
        return 0;
    endfunction

    function automatic logic g;
        automatic logic res = f();
        return res;
    endfunction
endpackage

module top;
    localparam A = PKG::g();
    initial $display("%b", A);
endmodule
