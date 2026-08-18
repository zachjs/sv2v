module top;
    wire [31:0] a1;
    wire [7:0] a2;
    if (1) begin : blk
        wire [31:0] b1;
        wire [7:0] b2;
        initial $display("b1 %b %b", b1, 1);
        initial $display("b2 %b %b", b2, 1);
    end
    initial $display("a1 %b %b", a1, 2);
    initial $display("a2 %b %b", a2, 8'sd2);
endmodule
