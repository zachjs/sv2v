module top;
    wire [31:0] out1, out2, out3, out4;
    mod #(1) m1(out1);
    mod #(2) m2(out2);
    mod #(3) m3(out3);
    mod #(4) m4(out4);
endmodule
