localparam Z = 1;
localparam Y = 5'(Z);
localparam X = 5'(Y);

module top;
    initial begin
        $display("%b", X);
        $display("%b", Y);
        $display("%b", Z);
    end
endmodule
