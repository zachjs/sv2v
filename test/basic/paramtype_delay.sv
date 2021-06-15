module mod #(
    parameter STR = "",
    parameter type T = logic,
    parameter WIDTH = 32,
    parameter type WIDTH_T = logic [WIDTH-1:0],
    parameter T INDIRECT = 0,
    parameter type OTHER_T = struct packed { type(INDIRECT) x, y; }
);
    initial begin
        $display("%s $bits(T) = %0d", STR, $bits(T));
        $display("%s WIDTH = %0d", STR, WIDTH);
        $display("%s $bits(WIDTH_T) = %0d", STR, $bits(WIDTH_T));
        $display("%s $bits(OTHER_T) = %0d", STR, $bits(OTHER_T));
    end
endmodule

module top;
    parameter type BASE = byte;
    typedef struct packed { BASE y; } W;
    W w;
    typedef struct packed { type(w) x, y; } V;
    V v;
    typedef logic [$bits(v)*2-1:0] U;
    U u;

    `define TEST(x) \
        assign x = 0; \
        mod #(`"x`", type(x)) m``x();
    `TEST(w)
    `TEST(v)
    `TEST(u)
    mod #("t") mt();
endmodule
