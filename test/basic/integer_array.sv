module top;
    parameter ONE = 1;
    localparam integer A [4] = { 1, 2, 3, 4 };
    localparam byte    B [4] = { 1, 2, 3, 4 };
    localparam bit     C [4] = { 1, 2, 3, 4 };
    localparam integer signed D [4] = { -1, -2, -3, -4 };
    localparam byte    signed E [4] = { -1, -2, -3, -4 };
    localparam bit     signed F [4] = { -1, -2, -3, -4 };
    localparam integer G [4] = '{ 1, 2, 3, 4 };
    localparam byte    H [4] = '{ 1, 2, 3, 4 };
    localparam bit     I [4] = '{ 1, 2, 3, 4 };
    localparam integer J [4] = { ONE * '0, ONE * '1, 5 * ONE, ONE * 6 };
    localparam byte    K [4] = { ONE * '0, ONE * '1, 5 * ONE, ONE * 6 };
    localparam bit     L [4] = { '0, ONE * '1, 5 * ONE, ONE * 6 };
    localparam integer unsigned M [4] = { ONE * '0, ONE * '1, 5 * ONE, ONE * 6 };
    localparam byte    unsigned N [4] = { ONE * '0, ONE * '1, 5 * ONE, ONE * 6 };
    localparam bit     unsigned O [4] = { '0, ONE * '1, 5 * ONE, ONE * 6 };
    initial begin
`define PRINT(X) \
        $display("%b %2d %2d", {X[0], X[1], X[2], X[3]}, $bits(X), $bits(X[0]));
        `PRINT(A);
        `PRINT(B);
        `PRINT(C);
        `PRINT(D);
        `PRINT(E);
        `PRINT(F);
        `PRINT(G);
        `PRINT(H);
        `PRINT(I);
        `PRINT(J);
        `PRINT(K);
        `PRINT(L);
        `PRINT(M);
        `PRINT(N);
        `PRINT(O);
    end

    localparam [1:0][0:1] P = '{'{default:'d1}, '{default:'d2}};
    localparam bit [1:0][0:1] Q = '{'{default:'d2}, '{default:'d1}};
    initial begin
        $display("%b %b %b", P, P[0], P[1]);
        $display("%b %b %b", Q, Q[0], Q[1]);
    end

    initial begin
        logic [1:0][0:1][7:0] a;
        a[0][0:1] = '{default: 1};
        $display("a: %b", a);
        a[1][0+:1] = '{default: 2};
        $display("a: %b", a);
        a[1][1-:1] = '{default: 3};
        $display("a: %b", a);
        a[1][1][3+:4] = '{default: '1};
        $display("a: %b", a);
    end
endmodule
