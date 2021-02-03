module top;
    reg [3:0] t;
    initial begin : blk
        reg [4:0] x;
        t = 1'sb1;
        x = t;
        $display("%b", t);
        $display("%b", x);
    end
endmodule
