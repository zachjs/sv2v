module mod(a, b, c, d);
    parameter P = 2;
    function automatic [P - 1:0] F;
        input signed inp;
        F = inp;
    endfunction
    parameter Q = 1;
    localparam R = F(Q) + Q;
    input signed a;
    input signed b;
    input signed c;
    input d;
    initial #1 $display("mod P=%0d Q=%0d R=%0d a=%b b=%b c=%b d=%b",
        P, Q, R, {P{a}}, {Q{b}}, {R{c}}, d);
endmodule

module top;
    parameter P = 1;
    parameter Q = 2;
    parameter R = 3;
    mod #() m1(1'b1, 1'bX, 1'bZ, 1'b1);
    mod #(P, Q) m2(1'b1, 1'b1, 1'b1, 1'b1);
    mod #(P, R) m3(1'b1, 1'b1, 1'b1, 1'b1);
    mod #(Q, R) m4(1'b1, 1'b1, 1'b1, 1'b1);
endmodule
