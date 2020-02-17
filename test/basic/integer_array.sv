module top;
    localparam integer B [4] = { 1, 2, 3, 4 };
    localparam byte    C [4] = { 1, 2, 3, 4 };
    localparam bit     D [4] = { 1, 2, 3, 4 };
    initial begin
`define PRINT(X) \
        $display("%b %2d %2d", {X[0], X[1], X[2], X[3]}, $bits(X), $bits(X[0]));
        `PRINT(B);
        `PRINT(C);
        `PRINT(D);
    end
endmodule
