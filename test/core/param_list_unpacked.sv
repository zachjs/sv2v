module fibA #(
    parameter integer N = 1,
    parameter integer W [2] = '{ 0, 1 }
);
    initial $display("fibA(%0d) = %0d", N, W[0]);
    if (N < 11)
        fibA #(N + 1, '{ W[1], W[0] + W[1] }) f();
endmodule
module fibB;
    parameter integer N = 1;
    parameter integer W [2] = '{ 0, 1 };
    initial $display("fibB(%0d) = %0d", N, W[0]);
    if (N < 11)
        fibB #(N + 1, '{ W[1], W[0] + W[1] }) f();
endmodule
module top;
    fibA fA();
    fibB fB();
endmodule
