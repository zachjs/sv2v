class C #(
    parameter PARAM_E = 1,
    parameter type PARAM_T = logic
);
    localparam E = PARAM_E * $bits(PARAM_T);
    localparam type T = PARAM_T [PARAM_E - 1:0];
endclass

package P;
    localparam E = 2;
    localparam type T = logic [E - 1:0];
endpackage

module M #(
    parameter DELAY,
    parameter PREFIX,
    parameter SUFFIX,
    parameter E,
    parameter type T
);
    initial #DELAY $display("M: %sE%s=%0d $bits(%sT%s)=%0d",
        PREFIX, SUFFIX, E, PREFIX, SUFFIX, $bits(T));
endmodule

interface I #(
    parameter DELAY,
    parameter PREFIX,
    parameter SUFFIX,
    parameter E,
    parameter type T
);
    initial #DELAY $display("I: %sE%s=%0d $bits(%sT%s)=%0d",
        PREFIX, SUFFIX, E, PREFIX, SUFFIX, $bits(T));
endinterface

module top;
    parameter FLAG = 1;

    localparam E = 3;
    localparam type T = logic [E * 2 - 1:0];

    `define TEST(D, prefix) \
        if (FLAG) begin \
            localparam the_expr = prefix``E; \
            localparam type the_type = prefix``T; \
            initial begin \
                #(D * 10); \
                $display(`"prefix``E = %0d %0d`", the_expr, prefix``E); \
                $display(`"$bits(prefix``E) = %0d %0d`", $bits(the_expr), $bits(prefix``E)); \
                $display(`"$bits(prefix``T) = %0d %0d`", $bits(the_type), $bits(prefix``T)); \
                $display(`"$left(prefix``E) = %0d`", $left(prefix``E)); \
                $display(`"$left(prefix``T) = %0d`", $left(prefix``T)); \
                $display(`"prefix``E'('z) = %b`", prefix``E'('z)); \
                $display(`"prefix``T'('z) = %b`", prefix``T'('z)); \
            end \
            I #(D*10+1, `"prefix`", "", prefix``E, prefix``T) i1(); \
            M #(D*10+2, `"prefix`", "", prefix``E, prefix``T) m1(); \
            I #(D*10+3, `"prefix`", "[6]", prefix``E[6], prefix``T[6]) i2(); \
            M #(D*10+4, `"prefix`", "[6]", prefix``E[6], prefix``T[6]) m2(); \
            I #(D*10+5, `"prefix`", "[6:0]", prefix``E[6:0], prefix``T[6:0]) i3(); \
            M #(D*10+6, `"prefix`", "[6:0]", prefix``E[6:0], prefix``T[6:0]) m3(); \
        end

    `TEST(0, )
    `TEST(1, P::)

    `TEST(2, C#()::)
    `TEST(3, C#(E, T)::)
    `TEST(4, C#(P::E, P::T)::)
    `TEST(5, C#(C#(E, T)::E, C#(E, T)::T)::)
    `TEST(6, C#(E[1], T[2])::)
    `TEST(7, C#(E[2:0], T[2:0])::)

endmodule
