module top;
    localparam i = 1234;
    function automatic integer f;
        input integer i;
        f = i + 1;
    endfunction
    initial $display(i, f(0), f(i + 1));
endmodule
