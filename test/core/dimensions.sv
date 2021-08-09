`define DUMP_DIM(typ, dim) \
        $display("    $size      %0d", $size     (typ, dim)); \
        $display("    $left      %0d", $left     (typ, dim)); \
        $display("    $right     %0d", $right    (typ, dim)); \
        $display("    $high      %0d", $high     (typ, dim)); \
        $display("    $low       %0d", $low      (typ, dim)); \
        $display("    $increment %0d", $increment(typ, dim));

`define EXHAUST(typ) \
    $display(`"Dumping info for typ`"); \
    $display("  $dimensions %0d", $dimensions(typ)); \
    $display("  $unpacked_dimensions %0d", $unpacked_dimensions(typ)); \
    $display("  $bits %0d", $bits(typ)); \
    $display("  1st dimension"); \
    `DUMP_DIM(typ, 1) \
    $display("  2nd dimension"); \
    `DUMP_DIM(typ, 2)

module top;
    typedef logic [16:1] Word;
    Word Ram[0:9];
    typedef logic [1:16] WordFlip;
    WordFlip RamFlip[9:0];
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

        $display("bits %0d", $bits("AB"));
        $display("bits %0d", $bits("A"));
        $display("bits %0d", $bits(""));

        $display("args %b", $size(RamPair, 1));
        $display("args %b", $size(RamPair, 1'b1));
        $display("args %b", $size(RamPair, 1'sb1));
        $display("args %b", $size(RamPair, 2'sb1));
        $display("args %b", $size(RamPair, 2'sb01));
        $display("args %b", $size(RamPair, 2'sb10));
        $display("args %b", $size(RamPair, 2'sb11));
        $display("args %b", $size(RamPair, '1));
        $display("args %b", $size(RamPair, 'o1));
        $display("args %b", $size(RamPair, 1'h1));
        $display("args %b", $size(RamPair, 1'd1));
        $display("args %b", $size(RamPair, 1'dx));
        $display("args %b", $size(RamPair, '0));
        $display("args %b", $size(RamPair, 'x));
        $display("args %b", $size(RamPair, $bits(integer) - 31));
        $display("args %b", $size(integer, $bits(integer) - 31));

        `EXHAUST(Ram);
        `EXHAUST(Ram[0]);
        `EXHAUST($unsigned(Ram[0]));
        `EXHAUST($signed(Ram[0]));
        `EXHAUST(Ram[0+:2]);
        `EXHAUST(Ram[1+:2]);
        `EXHAUST(Ram[2-:2]);
        `EXHAUST(Ram[3-:2]);
        `EXHAUST(Ram[0][2-:1]);
        `EXHAUST(Ram[0][2-:2]);

        `EXHAUST(RamFlip);
        `EXHAUST(RamFlip[0]);
        `EXHAUST($unsigned(RamFlip[0]));
        `EXHAUST($signed(RamFlip[0]));
        `EXHAUST(RamFlip[0+:2]);
        `EXHAUST(RamFlip[1+:2]);
        `EXHAUST(RamFlip[2-:2]);
        `EXHAUST(RamFlip[3-:2]);
        `EXHAUST(RamFlip[0][2-:1]);
        `EXHAUST(RamFlip[0][2-:2]);

        `EXHAUST(RamPair);
        `EXHAUST(RamPair[0]);
        `EXHAUST(Word);
        `EXHAUST(integer);
        `EXHAUST(byte);
        `EXHAUST(ints);
        `EXHAUST(ints_rev);
        `EXHAUST(unpacked_t);
        `EXHAUST(unpacked);
        `EXHAUST(type(foo[3:0]));
        `EXHAUST(type(ints[2][3:0]));
        `EXHAUST(type(ints[1:0]));
        `EXHAUST(type(ints_rev[0:1]));
        `EXHAUST(type(ints[2:1]));
        `EXHAUST(type(ints_rev[1:2]));

        `EXHAUST(int)
        `EXHAUST(shortint)
        `EXHAUST(longint)
        `EXHAUST(time)
    end
endmodule
