module mod #(
    parameter S = 1
);
    initial $display("$bits(T) = %0d", S);
endmodule

module top;
    parameter SIZE = 8;
    mod #(SIZE) m();
endmodule
