`define DUMP_DIM(left, right) \
        $display("    $size      %0d", \
            1 + (left >= right ? left - right : right - left)); \
        $display("    $left      %0d", left); \
        $display("    $right     %0d", right); \
        $display("    $high      %0d", left >= right ? left : right); \
        $display("    $low       %0d", left >= right ? right : left); \
        if (left === 1'bx) \
            $display("    $increment %0d", 1'bx); \
        else \
            $display("    $increment %0d", left >= right ? 1 : -1);

`define EXHAUST(typ,
    left1, right1,
    left2, right2,
    dimensions, unpacked_dimensions, bits) \
    $display(`"Dumping info for typ`"); \
    $display("  $dimensions %0d", dimensions); \
    $display("  $unpacked_dimensions %0d", unpacked_dimensions); \
    $display("  $bits %0d", bits); \
    $display("  1st dimension"); \
    `DUMP_DIM(left1, right1); \
    $display("  2nd dimension"); \
    `DUMP_DIM(left2, right2);

module top;
        reg [16:1] arr [0:0];
    initial begin
        $display(16);
        $display(16);
        $display(16);
        $display(3);

        $display("bits %0d", $bits("AB"));
        $display("bits %0d", $bits("A"));
        $display("bits %0d", $bits(""));

        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 1'bx);
        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 1'bx);
        $display("args %b", 1'bx);
        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 2);
        $display("args %b", 1'bx);
        $display("args %b", 1'bx);
        $display("args %b", 1'bx);
        $display("args %b", 2);
        $display("args %b", 32);

        `EXHAUST(Ram,
                0, 9,
                16, 1,
                2, 1, 160);
        `EXHAUST(Ram[0],
                16, 1,
                1'bx, 1'bx,
                1, 0, 16)
        `EXHAUST($unsigned(Ram[0]),
                15, 0,
                1'bx, 1'bx,
                1, 0, 16)
        `EXHAUST($signed(Ram[0]),
                15, 0,
                1'bx, 1'bx,
                1, 0, 16)
        `EXHAUST(Ram[0+:2],
                0, 1,
                16, 1,
                2, 1, 32)
        `EXHAUST(Ram[1+:2],
                1, 2,
                16, 1,
                2, 1, 32)
        `EXHAUST(Ram[2-:2],
                1, 2,
                16, 1,
                2, 1, 32)
        `EXHAUST(Ram[3-:2],
                2, 3,
                16, 1,
                2, 1, 32)
        `EXHAUST(Ram[0][2-:1],
                2, 2,
                1'bx, 1'bx,
                1, 0, 1)
        `EXHAUST(Ram[0][2-:2],
                2, 1,
                1'bx, 1'bx,
                1, 0, 2)

        `EXHAUST(RamFlip,
                9, 0,
                1, 16,
                2, 1, 160)
        `EXHAUST(RamFlip[0],
                1, 16,
                1'bx, 1'bx,
                1, 0, 16)
        `EXHAUST($unsigned(RamFlip[0]),
                15, 0,
                1'bx, 1'bx,
                1, 0, 16)
        `EXHAUST($signed(RamFlip[0]),
                15, 0,
                1'bx, 1'bx,
                1, 0, 16)
        `EXHAUST(RamFlip[0+:2],
                1, 0,
                1, 16,
                2, 1, 32)
        `EXHAUST(RamFlip[1+:2],
                2, 1,
                1, 16,
                2, 1, 32)
        `EXHAUST(RamFlip[2-:2],
                2, 1,
                1, 16,
                2, 1, 32)
        `EXHAUST(RamFlip[3-:2],
                3, 2,
                1, 16,
                2, 1, 32)
        `EXHAUST(RamFlip[0][2-:1],
                2, 2,
                1'bx, 1'bx,
                1, 0, 1)
        `EXHAUST(RamFlip[0][2-:2],
                1, 2,
                1'bx, 1'bx,
                1, 0, 2)

        `EXHAUST(RamPair,
                0, 1,
                0, 9,
                3, 2, 320)
        `EXHAUST(RamPair[0],
                0, 9,
                16, 1,
                2, 1, 160)
        `EXHAUST(Word,
                16, 1,
                1'bx, 1'bx,
                1, 0, 16)
        `EXHAUST(integer,
                31, 0,
                1'bx, 1'bx,
                1, 0, 32)
        `EXHAUST(byte,
                7, 0,
                1'bx, 1'bx,
                1, 0, 8)
        `EXHAUST(ints,
                3, 0,
                31, 0,
                2, 1, 128)
        `EXHAUST(ints_rev,
                0, 3,
                31, 0,
                2, 1, 128)
        `EXHAUST(unpacked_t,
                0, 2,
                7, 0,
                2, 1, 24)
        `EXHAUST(unpacked,
                0, 2,
                7, 0,
                2, 1, 24)
        `EXHAUST(type(foo[3:0]),
                3, 0,
                1'bx, 1'bx,
                1, 0, 4)
        `EXHAUST(type(ints[2][3:0]),
                3, 0,
                1'bx, 1'bx,
                1, 0, 4)
        `EXHAUST(type(ints[1:0]),
                1, 0,
                31, 0,
                2, 1, 64)
        `EXHAUST(type(ints_rev[0:1]),
                0, 1,
                31, 0,
                2, 1, 64)
        `EXHAUST(type(ints[2:1]),
                2, 1,
                31, 0,
                2, 1, 64)
        `EXHAUST(type(ints_rev[1:2]),
                1, 2,
                31, 0,
                2, 1, 64)

        `EXHAUST(int,
                31, 0,
                1'bx, 1'bx,
                1, 0, 32)
        `EXHAUST(shortint,
                15, 0,
                1'bx, 1'bx,
                1, 0, 16)
        `EXHAUST(longint,
                63, 0,
                1'bx, 1'bx,
                1, 0, 64)
        `EXHAUST(time,
                63, 0,
                1'bx, 1'bx,
                1, 0, 64)
    end
endmodule
