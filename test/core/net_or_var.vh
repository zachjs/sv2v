`define TEST_BASE(keywords, identifier) \
    keywords a_``identifier = 1'sb1; \
    keywords [1:0] a_ranged_``identifier = 1'sb1;

`define TEST_ALL(keywords, identifier) \
    `TEST_BASE(keywords, identifier) \
    keywords signed a_signed_``identifier = 1'sb1; \
    keywords unsigned a_unsigned_``identifier = 1'sb1; \
    keywords signed [1:0] a_signed_ranged_``identifier = 1'sb1; \
    keywords unsigned [1:0] a_unsigned_ranged_``identifier = 1'sb1;
