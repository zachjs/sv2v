module top;
    reg signed x;
    initial x = 1;
    parameter ONE = 1;
    initial begin
        localparam A = ONE * 1;
        localparam B = ONE * 2;
        localparam C = ONE * 3;
        localparam D = ONE * 4;
        localparam E = ONE * 5;
        $display("%b", 5'(4'(3'(2'(1'(x))))));
        $display("%b", E'(D'(C'(B'(A'(x))))));
        $display("%b", E'(D'(C'(B'(A'(E'(D'(C'(B'(A'(x)))))))))));
    end
endmodule
