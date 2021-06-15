module mod #(
    parameter type T = logic
);
    initial $display("$bits(T) = %0d", $bits(T));
endmodule

module top;
    parameter SIZE = 8;
    mod #(logic [SIZE-1:0]) m();
endmodule
