`define TEST(value) \
    logic [63:0] val_``value = 'value; \
    initial $display(`"'value -> %b %b", val_``value, 'value);

module top;
    `TEST(1);
    `TEST(0);
    `TEST(x);
    `TEST(z);
endmodule
