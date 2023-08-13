`define TEST(expr, size) \
    `TEST_INNER(expr, size) \
    `TEST_INNER(expr[2], size * 2) \
    `TEST_INNER(expr[2][3], size * 6) \
    `TEST_INNER(expr[3:7], size * 5) \
    `TEST_INNER(expr[$bits(T)], size) \
    `TEST_INNER(expr[$bits(U)], size * 2) \
    `TEST_INNER(expr[$bits(T[$bits(U)])], size * 2) \
    `TEST_INNER(expr[$bits(U[$bits(U)])], size * 4)

module top;
    `TEST(T, 1)
    `TEST(U, 2)
endmodule
