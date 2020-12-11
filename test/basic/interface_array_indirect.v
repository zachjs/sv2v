module top;
    initial $display("Group %0d", 8);
    generate
        genvar i;
        for (i = 0; i < 8; i = i + 1)
            initial #1 $display("Single %0d", i ** 3);
    endgenerate
endmodule
