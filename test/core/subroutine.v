module top;
`define TEST(subroutine) \
    initial repeat (4) $display(`"subroutine()`");
    `TEST(dump)
    `TEST(P::dump)
    `TEST(C#(1)::dump)
endmodule
