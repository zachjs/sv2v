module top;
    function automatic integer f;
        input integer inp;
        f = 1;
        for (integer idx = 0; idx < inp; idx = idx + 1) begin
            if (f == 32)
                break;
            f = f * 2;
        end
    endfunction
    function automatic integer g;
        input integer inp;
        integer idx;
        g = 1;
        for (idx = 0; idx < inp; idx = idx + 1) begin
            if (g == 32)
                break;
            g = g * 2;
        end
        g += idx;
    endfunction
    function automatic integer h;
        input integer inp;
        integer idx;
        h = 1;
        for (idx = 0; idx + 1 < 1 + inp; idx = idx + 1) begin
            if (h == 32)
                break;
            h = h * 2;
        end
        h += idx + 1;
    endfunction
    function automatic integer j;
        input integer inp;
        for (j = 0; j < inp; j = j + 1)
            if (j == 3)
                return j;
        j *= 2;
    endfunction
    function automatic integer k;
        input integer inp;
        for (k = 0; k + 1 < 1 + inp; k = k + 1)
            if (k == 3)
                return k * 3;
        k = k * 2 + 1;
    endfunction
    function automatic integer l;
        input integer inp;
        l = 1;
        for (integer idx = inp; 0 < idx; idx--) begin
            if (l == 32)
                break;
            l = l * 2;
        end
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
