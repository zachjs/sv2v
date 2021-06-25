module top;
    initial $display("%b", 2'b11);
    generate
        if (1) begin : blk2
            initial $display("%b", 3'b111);
        end
    endgenerate
endmodule
