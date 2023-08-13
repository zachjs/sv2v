`define TEST_INNER(expr, size) \
    initial $display(`"expr %0d %0d %0d`", \
        $bits(expr), $bits(type(expr)), size);
localparam type T = logic;
localparam type U = logic [2:1];
`include "typename_deep.svh"
