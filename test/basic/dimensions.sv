`define EXHAUST(t) \
        $display($size(t), $size(t,1), $size(t,2)); \
        $display($left(t), $left(t,1), $left(t,2)); \
        $display($right(t), $right(t,1), $right(t,2)); \
        $display($high(t), $high(t,1), $high(t,2)); \
        $display($low(t), $low(t,1), $low(t,2)); \
        $display($increment(t), $increment(t,1), $increment(t,2)); \
        $display($dimensions(t)); \
        $display($unpacked_dimensions(t)); \
        $display($bits(t));

module top;
    typedef logic [16:1] Word;
    Word Ram[0:9];
    type(Ram) RamPair [2];
    integer ints [3:0];
    integer ints_rev [0:3];
    typedef struct packed { logic x, y, z; } T;
    logic [$size(T)-1:0] foo;
    typedef byte unpacked_t [3];
    unpacked_t unpacked;
    initial begin
        $display($size(Word));
        $display($size(Ram,2));
        $display($size(Ram[0]));
        $display($bits(foo));

        `EXHAUST(Ram);
        `EXHAUST(Ram[0+:2]);
        `EXHAUST(Ram[1+:2]);
        `EXHAUST(Ram[0][2-:1]);
        `EXHAUST(RamPair);
        `EXHAUST(RamPair[0]);
        `EXHAUST(Word);
        `EXHAUST(integer);
        `EXHAUST(bit);
        `EXHAUST(byte);
        `EXHAUST(ints);
        `EXHAUST(ints_rev);
        `EXHAUST(unpacked_t);
        `EXHAUST(unpacked);
        `EXHAUST(type(foo[3:0]));
        `EXHAUST(type(ints[2][3:0]));
        `EXHAUST(type(ints[1:0]));
        `EXHAUST(type(ints_rev[0:1]));
    end
endmodule
