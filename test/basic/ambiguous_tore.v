module MI;
    parameter KIND = "";
    parameter DELAY = 0;
    parameter PREFIX = "";
    parameter SUFFIX = "";
    parameter E = 1;
    parameter T = 1;
    initial #DELAY $display("%s: %sE%s=%0d $bits(%sT%s)=%0d",
        KIND, PREFIX, SUFFIX, E, PREFIX, SUFFIX, T);
endmodule

module top;
    parameter FLAG = 1;

    `define TEST(D, prefix, eval, tval, leftval) \
        if (FLAG) begin \
            localparam [eval:1] ez = 1'sbz; \
            localparam [tval:1] tz = 1'sbz; \
            initial begin \
                #(D * 10); \
                $display(`"prefix``E = %0d %0d`", eval, eval); \
                $display(`"$bits(prefix``E) = %0d %0d`", 32, 32); \
                $display(`"$bits(prefix``T) = %0d %0d`", tval, tval); \
                $display(`"$left(prefix``E) = %0d`", 31); \
                $display(`"$left(prefix``T) = %0d`", leftval); \
                $display(`"prefix``E'('z) = %b`", ez); \
                $display(`"prefix``T'('z) = %b`", tz); \
            end \
            MI #("I", D*10+1, `"prefix`", "", eval, tval) i1(); \
            MI #("M", D*10+2, `"prefix`", "", eval, tval) m1(); \
            MI #("I", D*10+3, `"prefix`", "[6]", (eval >> 6) & 1'b1, tval*6) i2(); \
            MI #("M", D*10+4, `"prefix`", "[6]", (eval >> 6) & 1'b1, tval*6) m2(); \
            MI #("I", D*10+5, `"prefix`", "[6:0]", eval & 7'h7f, tval * 7) i3(); \
            MI #("M", D*10+6, `"prefix`", "[6:0]", eval & 7'h7f, tval * 7) m3(); \
        end

    `define TEST_C(D, prefix, efac, tfac) \
        `TEST(D, prefix, (efac)*(tfac), (tfac)*(efac), (efac)-1)

    `TEST(0, , 3, 6, 5)
    `TEST(1, P::, 2, 2, 1)

    `TEST_C(2, C#()::, 1, 1)
    `TEST_C(3, C#(E, T)::, 3, 6)
    `TEST_C(4, C#(P::E, P::T)::, 2, 2)
    `TEST_C(5, C#(C#(E, T)::E, C#(E, T)::T)::, 18, 18)
    `TEST_C(6, C#(E[1], T[2])::, 1, 12)
    `TEST_C(7, C#(E[2:0], T[2:0])::, 3, 18)

endmodule
