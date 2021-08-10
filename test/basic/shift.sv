`define TEST_OP(op, a, b) $display(`"%0d op %0d = %0d`", a, b, a op b)
`define TEST(a, b) \
    `TEST_OP(>> , a, b); \
    `TEST_OP(<< , a, b); \
    `TEST_OP(>>>, a, b); \
    `TEST_OP(<<<, a, b)

module top;
    initial begin
        `TEST(-4, 0);
        `TEST(-4, 1);
        `TEST(-4, 2);
        `TEST(-4, 3);

        `TEST(-1, 0);
        `TEST(-1, 1);
        `TEST(-1, 2);
        `TEST(-1, 3);

        `TEST(1, 0);
        `TEST(1, 1);
        `TEST(1, 2);
        `TEST(1, 3);

        `TEST(2, 0);
        `TEST(2, 1);
        `TEST(2, 2);
        `TEST(2, 3);
    end
endmodule
