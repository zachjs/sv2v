module top;
    initial begin : blk
        integer i;
        reg [1:0] y;
        reg x;
        for (i = 0; i < 3; i = i + 1) begin
            $display("%0d %b", 2, y);
            $display("%0d %b", 1, x);
        end
    end
endmodule
