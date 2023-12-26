module top;
    logic x;
    always_comb
        x = 0;

    `include "always_comb.vh"
    `TEST(_comb, 1)
    `TEST(_comb, 2)
    `TEST(@*, 3)
    `TEST(@*, 4)

    initial x1 = 0;
    initial x3 = 0;
endmodule
