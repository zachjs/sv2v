`define TEST(value) \
    wire [63:0] val_``value = {64{1'b``value}}; \
    initial $display(`"'value -> %b %b", val_``value, 1'b``value);

module top;
    `TEST(1)
    `TEST(0)
    `TEST(x)
    `TEST(z)
endmodule
