`define TEST_SG(port_sg, data_sg, exp_u, exp_s) \
    `TEST_RAW(o_``port_sg``_``data_sg``_one, port_sg      , data_sg        , [ 0:0], exp_u) \
    `TEST_RAW(o_``port_sg``_``data_sg``_two, port_sg [1:0], data_sg [1:0]  , [ 1:0], exp_u) \
    `TEST_RAW(o_``port_sg``_``data_sg``_int, port_sg      , integer data_sg, [31:0], exp_s)

`define TEST \
    `TEST_SG(        ,         , 0, 1) \
    `TEST_SG(        ,   signed, 1, 1) \
    `TEST_SG(        , unsigned, 0, 0) \
    `TEST_SG(  signed,         , 1, 1) \
    `TEST_SG(  signed,   signed, 1, 1) \
    `TEST_SG(  signed, unsigned, 1, 0) \
    `TEST_SG(unsigned,         , 0, 1) \
    `TEST_SG(unsigned,   signed, 1, 1) \
    `TEST_SG(unsigned, unsigned, 0, 0)

`define TEST_RAW(name, a, b, c, d) name,

module top(
    `TEST
    foo
);
    output wire foo;
    assign foo = 1;

`undef TEST_RAW
`define TEST_RAW(name, port, data, range, exp) \
    `ifdef REF \
        output wire range name; \
        initial #1 $display(`"name %b %b`", name, 1'b``exp); \
    `else \
        output port name; \
        wire data name; \
        initial #1 $display(`"name %b %b`", name, name < 0); \
    `endif \
    assign name = 1'sb1; \

    `TEST
endmodule
