module top;
`define TEST(sense) always sense $display(`"sense %b`", x);
    reg x, y;
    `TEST(@*)
    `TEST(@ *)
    `TEST(@x)
    `TEST(@y)
    `TEST(@ ( * ))
    `TEST(@ ( *))
    `TEST(@ (* ))
    `TEST(@ (*))
    `TEST(@( * ))
    `TEST(@( *))
    `TEST(@(* ))
    `TEST(@(*))
    initial x = 1;
endmodule
