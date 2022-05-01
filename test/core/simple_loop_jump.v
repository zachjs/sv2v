module top;
    function automatic integer f;
        input integer inp;
        if (inp > 5)
            f = 32;
        else
            f = 2 ** inp;
    endfunction
    function automatic integer g;
        input integer inp;
        if (inp > 5)
            g = 32 + 5;
        else
            g = 2 ** inp + inp;
    endfunction
    function automatic integer h;
        input integer inp;
        if (inp > 5)
            h = 32 + 5 + 1;
        else
            h = 2 ** inp + inp + 1;
    endfunction
    function automatic integer j;
        input integer inp;
        if (inp > 3)
            j = 3;
        else
            j = inp * 2;
    endfunction
    function automatic integer k;
        input integer inp;
        if (inp > 3)
            k = 3 * 3;
        else
            k = inp * 2 + 1;
    endfunction
    function automatic integer l;
        input integer inp;
        if (inp > 5)
            l = 32;
        else
            l = 2 ** inp;
    endfunction
    integer i;
    initial
        for (i = 0; i < 10; i = i + 1) begin
            $display("f(%0d) = %0d", i, f(i));
            $display("g(%0d) = %0d", i, g(i));
            $display("h(%0d) = %0d", i, h(i));
            $display("j(%0d) = %0d", i, j(i));
            $display("k(%0d) = %0d", i, k(i));
            $display("l(%0d) = %0d", i, l(i));
        end
endmodule
