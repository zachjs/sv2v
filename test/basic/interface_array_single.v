module top;
    genvar i;
    generate

        initial #1;

        `define PRINT(X, offset) \
        for (i = 4; i <= 8; i = i + 1) \
            initial begin \
                $display(`"Module``X got %0d`", i ** 2 + offset); \
                $display("ModuleN got %0d", i ** 2 + offset); \
            end

        `PRINT(A, 0)
        `PRINT(A, 1)
        `PRINT(A, 2)

        `PRINT(B, 1)
        `PRINT(B, 1)

        `PRINT(C, 2)
        `PRINT(C, 2)

    endgenerate
endmodule
