`define TEST(expr) initial begin : \blk``expr \
    localparam X = expr; \
    localparam [63:0] Y = X; \
    $display(`"expr %b %b %b`", expr, X, Y); \
end

`define TEST_LOGOP(op) \
    `TEST(0``op``0) \
    `TEST(0``op``1) \
    `TEST(0``op``1'bx) \
    `TEST(0``op``P) \
    `TEST(0``op``Q) \
    `TEST(0``op``R) \
    `TEST(1``op``0) \
    `TEST(1``op``1) \
    `TEST(1``op``1'bx) \
    `TEST(1``op``P) \
    `TEST(1``op``Q) \
    `TEST(1``op``R) \
    `TEST(1'bx``op``0) \
    `TEST(1'bx``op``1) \
    `TEST(1'bx``op``1'bx) \
    `TEST(1'bx``op``P) \
    `TEST(1'bx``op``Q) \
    `TEST(1'bx``op``R)

module top;
    `TEST((12'hb03-12'hb07)+12'hb10)
    parameter P = 0;
    parameter Q = 1;
    parameter R = 1'bx;
    `TEST_LOGOP(||)
    `TEST_LOGOP(&&)
endmodule
