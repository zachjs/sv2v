module fib #(
    parameter VARIANT = "",
    parameter N = 1,
    parameter W0 = 0,
    parameter W1 = 1
);
    initial $display("fib%s(%0d) = %0d", VARIANT, N, W0);
    if (N < 11)
        fib #(VARIANT, N + 1, W1, W0 + W1) f();
endmodule
module top;
    fib #("A") fA();
    fib #("B") fB();
endmodule
