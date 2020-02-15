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

typedef enum logic {
    F_1, F_2
} EnumF;

typedef enum [0:0] {
    G_1, G_2
} EnumG;

typedef logic [3:0] Foo_t;
typedef enum Foo_t {
    H_1 = 'b1, H_2 = 'b0
} EnumH;

typedef enum int {
    I_1, I_2
} EnumI;

typedef enum int {
    J_1, J_2, J_3
} EnumJ;

typedef enum int {
    Z_1, Z_2, Z_3
} EnumZ;

`define PRINT(name, val) \
    dummy``name = name``_``val; \
    $display("%h %h %0d %0d", \
        name``_``val, dummy``name, $bits(name``_``val), $bits(dummy``name));

`define PRINT_UNSIZED(name, val) \
    dummy``name = name``_``val; \
    $display("%h %h %0d", \
        name``_``val, dummy``name, $bits(dummy``name));

module top;
    EnumA dummyA;
    EnumB dummyB;
    EnumC dummyC;
    EnumD dummyD;
    EnumE dummyE;
    EnumF dummyF;
    EnumG dummyG;
    EnumH dummyH;
    EnumI dummyI;

    initial begin

        `PRINT(A, 1)
        `PRINT(A, 2)
        `PRINT(A, 3)

        `PRINT(B, 1)
        `PRINT(B, 2)
        `PRINT(B, 3)

        `PRINT(C, 1)
        `PRINT(C, 2)
        `PRINT(C, 3)

        `PRINT(D, 1)
        `PRINT(D, 2)
        `PRINT(D, 3)

        `PRINT(E, 1)
        `PRINT(E, 2)
        `PRINT(E, 3)
        `PRINT(E, 4)
        `PRINT(E, 5)
        `PRINT(E, 6)

        `PRINT(F, 1)
        `PRINT(F, 2)

        `PRINT(G, 1)
        `PRINT(G, 2)

        `PRINT_UNSIZED(H, 1)
        `PRINT_UNSIZED(H, 2)

        `PRINT_UNSIZED(I, 1)
        `PRINT_UNSIZED(I, 2)

    end

    parameter USE_J = 1;
    generate
        if (USE_J) begin
            EnumJ dummyJ;
            initial begin
                `PRINT(J, 1)
                `PRINT(J, 2)
                `PRINT(J, 3)
            end
        end
        else begin
            EnumZ dummyZ;
            initial begin
                `PRINT(Z, 1)
                `PRINT(Z, 2)
                `PRINT(Z, 3)
            end
        end
    endgenerate

endmodule
