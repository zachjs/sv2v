`define PRINT(val) $display("%02d", val);

module top;
    initial begin

        `PRINT(0)
        `PRINT(1)
        `PRINT(2)

        `PRINT(2)
        `PRINT(1)
        `PRINT(3)

        `PRINT(20)
        `PRINT(0)
        `PRINT(19)

        `PRINT(16)
        `PRINT(17)
        `PRINT(18)

        `PRINT(0)
        `PRINT(16)
        `PRINT(17)
        `PRINT(18)
        `PRINT(2)
        `PRINT(3)

    end
endmodule
