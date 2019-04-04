typedef enum {
    A_1, A_2, A_3
} EnumA;

typedef enum {
    B_1 = 2, B_2 = 1, B_3 = 3
} EnumB;

typedef enum {
    C_1 = 20, C_2 = 0, C_3 = 19
} EnumC;

typedef enum {
    D_1 = 'h10, D_2, D_3
} EnumD;

typedef enum {
    E_1, E_2 = 'h10, E_3, E_4, E_5 = 'b10, E_6
} EnumE;

`define PRINT(val) $display("%02d", val);

module top;
    EnumA dummyA;
    EnumB dummyB;
    EnumC dummyC;
    EnumD dummyD;
    EnumE dummyE;

    initial begin

        `PRINT(A_1)
        `PRINT(A_2)
        `PRINT(A_3)

        `PRINT(B_1)
        `PRINT(B_2)
        `PRINT(B_3)

        `PRINT(C_1)
        `PRINT(C_2)
        `PRINT(C_3)

        `PRINT(D_1)
        `PRINT(D_2)
        `PRINT(D_3)

        `PRINT(E_1)
        `PRINT(E_2)
        `PRINT(E_3)
        `PRINT(E_4)
        `PRINT(E_5)
        `PRINT(E_6)

    end
endmodule
