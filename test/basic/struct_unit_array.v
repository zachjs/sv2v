module top;
    wire [2:0] s = 3'b110;
    initial #1 $display("%b", s);
endmodule
