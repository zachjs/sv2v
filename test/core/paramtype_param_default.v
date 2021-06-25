module M #(
    parameter ID = "Z",
    parameter K = 2,
    parameter T = K
);
    initial $display("%s %0d %0d", ID, K, T);
endmodule

module top;
    M                    z();
    M #(.ID("A"))        a();
    M #(.ID("B"), .K(3)) b();
    M #(.ID("C"), .K(4)) c();
    M #(.ID("D"), .K(4), .T(8)) d();
endmodule
