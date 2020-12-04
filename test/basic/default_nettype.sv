`resetall

`define TEST(t) \
    `default_nettype t \
    module test_``t; \
        assign foo = 0; \
    endmodule

module top;
    assign foo = 0;
endmodule

`TEST(tri)
`TEST(triand)
`TEST(trior)
// `TEST(trireg)
`TEST(tri0)
`TEST(tri1)
// `TEST(uwire)
`TEST(wire)
`TEST(wand)
`TEST(wor)
