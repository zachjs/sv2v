module Example;
    wire [3:0] v = 4'b1111;
    initial #1 $display("%b", v);
endmodule
module top; Example example(); endmodule
