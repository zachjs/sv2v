`define TEST_INNER(expr, size) \
    initial $display(`"expr %0d %0d %0d`", \
        size, size, size);
`include "typename_deep.svh"
