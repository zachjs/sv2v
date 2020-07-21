module top;
    function automatic f;
        input unused;
        f = 0;
    endfunction

    function automatic g;
        input unused;
        g = f(0);
    endfunction

    localparam A = g(0);
    initial $display("%b", A);
endmodule
