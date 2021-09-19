module top;
    initial $display("p %0d %b", 32, 32'd1);
    initial $display("q %0d %b", 32, 32'd2);
    initial $display("p %0d", 64);
    initial $display("q %0d", 64);
    initial $display("p %0d %b", 2, 2'd1);
    initial $display("q %0d %b", 2, 2'd2);
    initial $display("p %0d", 4);
    initial $display("q %0d", 4);
endmodule
