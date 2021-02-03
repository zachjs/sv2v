`define TEST(expr) \
    `TEST_CAST(expr, w, t1) \
    `TEST_CAST(expr, w, t2) \
    `TEST_CAST(expr, w, t3) \
    `TEST_CAST(expr, w, s1) \
    `TEST_CAST(expr, w, s2) \
    `TEST_CAST(expr, w, s3) \
    `TEST_CAST(expr, r, t1) \
    `TEST_CAST(expr, r, t2) \
    `TEST_CAST(expr, r, t3) \
    `TEST_CAST(expr, r, s1) \
    `TEST_CAST(expr, r, s2) \
    `TEST_CAST(expr, r, s3)

wire wt1;
wire signed wt2;
wire unsigned wt3;

wire [1:0] ws1;
wire signed [1:0] ws2;
wire unsigned [1:0] ws3;

reg rt1;
reg signed rt2;
reg unsigned rt3;

reg [1:0] rs1;
reg signed [1:0] rs2;
reg unsigned [1:0] rs3;

reg [63:0] tmp;
