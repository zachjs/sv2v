module top;
    `include "always_spin.vh"
    logic [Z - 1:0] foo;
    logic flag;
    logic bar;
    always_comb
        bar = foo[Z - 1] & flag;
endmodule
