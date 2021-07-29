module mod(a, b, c, d);
    parameter P = 2;
    function automatic [P - 1:0] F;
        input signed inp;
        F = inp;
    endfunction
    parameter Q = 1;
    if (1) begin : blk
        localparam X = F(Q) + Q;
        wire [X - 1:0] g;
    end
    wire x = blk.g;
    localparam R = $bits(blk.g);
    input logic [P - 1:0] a;
    input wire [Q - 1:0] b;
    input [R - 1:0] c;
    input d;
    initial #1 $display("mod P=%0d Q=%0d R=%0d a=%b b=%b c=%b d=%b",
        P, Q, R, a, b, c, d);
endmodule

module top;
    parameter P = 1;
    parameter Q = 2;
    parameter R = 3;
    mod #() m1('1, 'X, 'Z, '1);
    mod #(P, Q) m2('1, '1, '1, '1);
    mod #(P, R) m3('1, '1, '1, '1);
    mod #(Q, R) m4('1, '1, P ? '1 : 'X, '1);
endmodule
