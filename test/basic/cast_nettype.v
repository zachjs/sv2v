`define TEST_CAST(expr, prefix, typ) \
    initial begin \
        r``typ = expr; \
        tmp = r``typ; \
        $display(`"%b => prefix``typ %b %b %b`", expr, r``typ, tmp, r``typ); \
    end

module top;
    wire foo;
    reg bar;
    initial bar = 1;

    `include "cast_nettype.vh"

    `TEST(1'sb1)
    `TEST(1'sbx)
    `TEST(1)
    `TEST(2)
    `TEST(-1)
endmodule
