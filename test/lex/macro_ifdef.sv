`define FELSE `else

`define MACRO_A(flag, check1 = ifdef, check2 = else) \
    initial $display( \
    `check1 flag \
    `"flag is check1'd`" \
    `check2 \
    `"flag is not check1'd`" \
    `endif \
    );

`define A

module top;
    `MACRO_A(A)
    `MACRO_A(B)
    `MACRO_A(A, ifndef)
    `MACRO_A(B, ifndef)
    `MACRO_A(A, ifdef, FELSE)
    `MACRO_A(B, ifdef, FELSE)
    `MACRO_A(A, ifndef, FELSE)
    `MACRO_A(B, ifndef, FELSE)
endmodule
