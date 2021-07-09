module top;
`define TEST(kw) kw a_``kw;
    `TEST(supply0)
    `TEST(supply1)
    `TEST(tri)
    `TEST(triand)
    `TEST(trior)
    `TEST(trireg)
    `TEST(tri0)
    `TEST(tri1)
    `TEST(uwire)
    `TEST(wire)
    `TEST(wand)
    `TEST(wor)
endmodule
