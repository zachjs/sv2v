`define PRINT(str, num) $display(`"StrIs``str NumIs``num`");

module top;
    initial begin

        `PRINT(FOO, 0)
        `PRINT(BAR, 1)
        `PRINT(BAZ, 2)

        `PRINT(A, 1)
        `PRINT(B, 2)
        `PRINT(C, 3)

    end
endmodule
