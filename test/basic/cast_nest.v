module top;
    reg signed x;
    initial x = 1;
    parameter ONE = 1;
    initial begin : blk
        reg signed [4:0] y;
        y = x;
        $display("%b", y);
        $display("%b", y);
        $display("%b", y);
    end
endmodule
