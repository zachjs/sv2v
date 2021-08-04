module example;
    parameter P = 0;

`define BLK genblk1
         if (P == 1) integer w = 1;
    else if (P == 2) integer x = 2;
    else if (P == 3) integer y = 3;
    else             integer z = 9;

`undef BLK
`define BLK genblk2
    case (P)
        1      : integer w = 1;
        2      : integer x = 2;
        3      : integer y = 3;
        default: integer z = 9;
    endcase

`undef BLK
`define BLK genblk3
    if (1) wire a = 1;
endmodule

module top;
`define TEST(i, v) \
    example #(i) e``i(); \
    initial #i begin \
        $display(`"e``i.genblk1.v: %0d`", e``i.genblk1.v); \
        $display(`"e``i.genblk2.v: %0d`", e``i.genblk2.v); \
        $display(`"e``i.genblk3.a: %0d`", e``i.genblk3.a); \
    end
    `TEST(1, w)
    `TEST(2, x)
    `TEST(3, y)
    `TEST(4, z)
endmodule
