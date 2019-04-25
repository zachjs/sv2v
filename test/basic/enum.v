`define PRINT(name, val) \
    $display("%h %h %0d %0d", \
        val, val, $bits(dummy``name), $bits(dummy``name));

module top;

    reg [31:0] dummyA;
    reg [31:0] dummyB;
    reg [31:0] dummyC;
    reg [31:0] dummyD;
    reg [31:0] dummyE;
    reg [0:0] dummyF;
    reg [0:0] dummyG;

    initial begin

        dummyA = 'b10;
        dummyB = 'b11;
        dummyC = 'b10011;
        dummyD = 'b10010;
        dummyE = 'b11;
        dummyF = 'b1;
        dummyG = 'b1;

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

    end
endmodule

