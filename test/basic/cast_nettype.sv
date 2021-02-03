`define TEST_CAST(expr, prefix, typ) \
    initial begin \
        localparam type T = type(prefix``typ); \
        logic [63:0] x; \
        T y; \
        x = T'(expr); \
        y = expr; \
        r``typ = expr; \
        tmp = r``typ; \
        $display(`"%b => prefix``typ %b %b %b`", expr, T'(expr), x, y); \
    end

module top;
    wire foo;
    type(foo) bar;
    initial bar = 1;

    `include "cast_nettype.vh"

    `TEST('1)
    `TEST('x)
    `TEST(1)
    `TEST(2)
    `TEST(-1)
endmodule
