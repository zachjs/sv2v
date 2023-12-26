module top;
    reg x;
    initial
        x = 0;

    `include "always_comb.vh"
    `TEST(@(x1), 1)
    `TEST(@(x1), 2)
    `TEST(@*, 3)
    `TEST(@*, 4)

    initial x1 = 0;
    initial x3 = 0;
endmodule
