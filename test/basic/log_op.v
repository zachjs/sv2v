module top;
    function log_imp;
        input integer a;
        input integer b;
        log_imp = !a || b;
    endfunction
    function log_eq;
        input integer a;
        input integer b;
        log_eq = !a == !b;
    endfunction
    initial begin : foo
        integer a, b;
        for (a = -2; a <= 2; a = a + 1)
            for (b = -2; b <= 2; b = b + 1)
                $display(log_imp(a, b), log_eq(a, b));
    end
endmodule
