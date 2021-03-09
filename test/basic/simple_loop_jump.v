module top;
    function automatic integer f;
        input integer inp;
        if (inp > 5)
            f = 32;
        else
            f = 2 ** inp;
    endfunction
    integer i;
    initial
        for (i = 0; i < 10; i = i + 1)
            $display("f(%0d) = %0d", i, f(i));
endmodule
