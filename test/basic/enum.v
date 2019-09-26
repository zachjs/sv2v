`define PRINT(name, val) \
    dummy``name = val; \
    $display("%h %h %0d %0d", \
        val, dummy``name, $bits(val), $bits(dummy``name));

`define PRINT_UNSIZED(name, val) \
    dummy``name = val; \
    $display("%h %h %0d", \
        val, dummy``name, $bits(dummy``name));

module top;

    reg [31:0] dummyA;
    reg [31:0] dummyB;
    reg [31:0] dummyC;
    reg [31:0] dummyD;
    reg [31:0] dummyE;
    reg [0:0] dummyF;
    reg [0:0] dummyG;
    reg [3:0] dummyH;
    reg [31:0] dummyI;

    initial begin

        `PRINT(A, 0)
        `PRINT(A, 1)
        `PRINT(A, 2)

        `PRINT(B, 2)
        `PRINT(B, 1)
        `PRINT(B, 3)

        `PRINT(C, 20)
        `PRINT(C, 0)
        `PRINT(C, 19)

        `PRINT(D, 16)
        `PRINT(D, 17)
        `PRINT(D, 18)

        `PRINT(E, 0)
        `PRINT(E, 16)
        `PRINT(E, 17)
        `PRINT(E, 18)
        `PRINT(E, 2)
        `PRINT(E, 3)

        `PRINT(F, 1'b0)
        `PRINT(F, 1'b1)

        `PRINT(G, 1'b0)
        `PRINT(G, 1'b1)

        `PRINT_UNSIZED(H, 'b1)
        `PRINT_UNSIZED(H, 'b0)

        `PRINT_UNSIZED(I, 'b0)
        `PRINT_UNSIZED(I, 'b1)

    end
endmodule

