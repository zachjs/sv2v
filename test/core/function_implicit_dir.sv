module top;
    function automatic integer f(
        input a, b, [1:0] c, d
    );
        f = {1'bx, a, 1'bx, b, 1'bx, c, 1'bx, d, 1'bx};
    endfunction
    integer x = f(0, 1, 2, 3);
    initial $display("%b", x);
endmodule
