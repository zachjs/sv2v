module example;
    parameter P = 0;

// needed because of steveicarus/iverilog#528
`ifdef __ICARUS__
    `define BEGIN begin : `BLK
    `define END end
`else
    `define BEGIN
    `define END
`endif

`define BLK genblk1
         if (P == 1) `BEGIN integer w = 1; `END
    else if (P == 2) `BEGIN integer x = 2; `END
    else if (P == 3) `BEGIN integer y = 3; `END
    else             `BEGIN integer z = 9; `END

`undef BLK
`define BLK genblk2
    case (P)
        1      : `BEGIN integer w = 1; `END
        2      : `BEGIN integer x = 2; `END
        3      : `BEGIN integer y = 3; `END
        default: `BEGIN integer z = 9; `END
    endcase

`undef BLK
`define BLK genblk3
    if (1) `BEGIN wire a = 1; `END
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
