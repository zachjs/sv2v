module top;
    localparam [0:127] A = { 32'h1, 32'h2, 32'h3, 32'h4 };
    localparam [0:31] B = { 8'h1, 8'h2, 8'h3, 8'h4 };
    localparam [0:3] C = { 1'h1, 1'h0, 1'h1, 1'h0 };
    localparam [0:127] D = { 32'hFFFFFFFF, 32'hFFFFFFFE, 32'hFFFFFFFD, 32'hFFFFFFFC };
    localparam [0:31] E = { 8'hFF, 8'hFE, 8'hFD, 8'hFC };
    localparam [0:3] F = { 1'h1, 1'h0, 1'h1, 1'h0 };
    localparam [0:127] G = { 32'h1, 32'h2, 32'h3, 32'h4 };
    localparam [0:31] H = { 8'h1, 8'h2, 8'h3, 8'h4 };
    localparam [0:3] I = { 1'h1, 1'h0, 1'h1, 1'h0 };
    localparam [0:127] J = { 32'h0, 32'hFFFFFFFF, 32'h5, 32'h6 };
    localparam [0:31] K = { 8'h0, 8'hFF, 8'h5, 8'h6 };
    localparam [0:3] L = { 1'h0, 1'h1, 1'h1, 1'h0 };
    localparam [0:127] M = { 32'h0, 32'hFFFFFFFF, 32'h5, 32'h6 };
    localparam [0:31] N = { 8'h0, 8'hFF, 8'h5, 8'h6 };
    localparam [0:3] O = { 1'h0, 1'h1, 1'h1, 1'h0 };
    initial begin
        $display("%b %2d %2d", A, $bits(A), 32);
        $display("%b %2d %2d", B, $bits(B), 8);
        $display("%b %2d %2d", C, $bits(C), 1);
        $display("%b %2d %2d", D, $bits(D), 32);
        $display("%b %2d %2d", E, $bits(E), 8);
        $display("%b %2d %2d", F, $bits(F), 1);
        $display("%b %2d %2d", G, $bits(G), 32);
        $display("%b %2d %2d", H, $bits(H), 8);
        $display("%b %2d %2d", I, $bits(I), 1);
        $display("%b %2d %2d", J, $bits(J), 32);
        $display("%b %2d %2d", K, $bits(K), 8);
        $display("%b %2d %2d", L, $bits(L), 1);
        $display("%b %2d %2d", M, $bits(M), 32);
        $display("%b %2d %2d", N, $bits(N), 8);
        $display("%b %2d %2d", O, $bits(O), 1);
    end

    localparam [3:0] P = 4'b1100;
    localparam [3:0] Q = 4'b0011;
    initial begin
        $display("%b %b %b", P, P[1:0], P[3:2]);
        $display("%b %b %b", Q, Q[1:0], Q[3:2]);
    end

    initial begin : block
        reg [31:0] a;
        a[7:0] = 1;
        a[15:8] = 1;
        $display("a: %b", a);
        a[31:24] = 2;
        $display("a: %b", a);
        a[23:16] = 3;
        $display("a: %b", a);
        a[23:19] = 4'b1111;
        $display("a: %b", a);
    end
endmodule
