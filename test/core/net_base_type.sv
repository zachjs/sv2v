`ifdef REF
    `define TEST(kw, name, conv) \
        wire conv wire_``name = 1'sb1; \
        wire [63:0] wire_``name``_ext = wire_``name;
`else
    `define TEST(kw, name, conv) \
        wire kw wire_``name = 1'sb1; \
        wire [63:0] wire_``name``_ext = wire_``name;
`endif

module top;
    `TEST(reg, reg, )
    `TEST(bit, bit, )
    `TEST(logic, logic, )
    `TEST(integer, integer, signed [31:0])
    `TEST(int, int, signed [31:0])
    `TEST(shortint, shortint, signed [15:0])
    `TEST(byte, byte, signed [7:0])
    `TEST(integer unsigned, integer_unsigned, [31:0])
    `TEST(int unsigned, int_unsigned, [31:0])
    `TEST(shortint unsigned, shortint_unsigned, [15:0])
    `TEST(byte unsigned, byte_unsigned, [7:0])
endmodule
