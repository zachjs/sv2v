module top;
    localparam [3:0] A = 0, B = 1, C = 2;
    localparam [7:0] b = B;
    localparam [7:0] c = C;
    initial begin
        #1;
        $display("%0d %0d %0d %0d %0d", A, B, C, b, c);
    end
endmodule
