`define TEST(aVal, bVal, cVal, expr) \
    if (PARAM) begin \
        wire [7:0] a, b; \
        wire [31:0] c; \
        assign a = aVal; \
        assign b = bVal; \
        assign c = cVal; \
        initial `BASE(expr, {a, b, c}, a, b, c); \
    end
`include "pattern_revised.sv"
