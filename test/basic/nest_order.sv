localparam Z = 1;
localparam Y = 5'(Z);
localparam X = 5'(Y);

module top;
    parameter W = 3;
    typedef enum logic [W-1:0] {
        A, B
    } E;
    initial begin
        $display("%b", A);
        $display("%b", B);
    end

    initial begin
        $display("%b", X);
        $display("%b", Y);
        $display("%b", Z);
    end
endmodule
