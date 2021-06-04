module mod #(
    parameter STR = "",
    parameter T = 1
);
    initial begin
        $display("%s $bits(T) = %0d", STR, T);
        $display("%s WIDTH = %0d", STR, 32);
        $display("%s $bits(WIDTH_T) = %0d", STR, 32);
        $display("%s $bits(OTHER_T) = %0d", STR, 2 * T);
    end
endmodule

module top;
    `define TEST(x, w) \
        wire [w-1:0] x; \
        assign x = 0; \
        mod #(`"x`", w) m``x();
    `TEST(w, 8)
    `TEST(v, 16)
    `TEST(u, 32)
    mod #("t") mt();
endmodule
