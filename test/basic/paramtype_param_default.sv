module M #(
    parameter ID = "Z",
    parameter K = 2,
    parameter type T = logic [K-1:0]
);
    initial $display("%s %0d %0d", ID, K, $bits(T));
endmodule

module top;
    M                    z();
    M #(.ID("A"))        a();
    M #(.ID("B"), .K(3)) b();
    M #(.ID("C"), .K(4)) c();
    parameter K = 4;
    localparam type T = logic [2*K-1:0];
    M #(.ID("D"), .K(4), .T(T)) d();
endmodule
