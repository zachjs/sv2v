module top;
    typedef enum logic [3:0] {
        A = (4'd15 + 4'd1) >> 1, B, C
    } T;
    localparam [7:0] b = B;
    localparam [7:0] c = C;
    initial begin
        #1;
        $display("%0d %0d %0d %0d %0d", A, B, C, b, c);
    end
endmodule
