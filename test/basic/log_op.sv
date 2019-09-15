module top;
    function log_imp;
        input integer a;
        input integer b;
        return a -> b;
    endfunction
    function log_eq;
        input integer a;
        input integer b;
        return a <-> b;
    endfunction
    initial
        for (integer a = -2; a <= 2; a++)
            for (integer b = -2; b <= 2; b++)
                $display(log_imp(a, b), log_eq(a, b));
endmodule
