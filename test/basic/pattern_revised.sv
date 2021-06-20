module top;
    parameter PARAM = 1;

`define BASE(expr, full, x, y, z) \
    $display(`"%b %0d %0d %0d expr`", \
        full, x, y, z)

`ifndef TEST
    typedef byte T;
    typedef struct packed {
        byte x;
        T y;
        integer z;
    } S;

`define TEST(a, b, c, expr) \
    if (PARAM) begin \
        S s; \
        assign s = expr; \
        initial `BASE(expr, s, s.x, s.y, s.z); \
    end
`endif

    `TEST(1, 2, 3, '{ x: 1, y: 2, z: 3 })
    `TEST(2, 2, 3, '{ byte: 2, integer: 3 })
    `TEST(3, 3, 2, '{ integer: 2, byte: 3 })
    `TEST(4, 4, 2, '{ integer: 2, T: 4 })
    `TEST(5, 5, 2, '{ integer: 2, T: 4, byte: 5 })
    `TEST(5, 5, 2, '{ 2: 2, byte: 5 })
    `TEST(7, 8, 9, '{ 1: 8, 2: 9, 0: 7 })
endmodule
