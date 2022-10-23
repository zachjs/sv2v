module top;
    `include "always_spin.vh"
    wire [Z - 1:0] foo;
    wire flag;
    reg bar;
    always @*
        bar = foo[Z - 1] & flag;
endmodule
