typedef integer integer_t;
typedef shortint shortint_t;
typedef byte byte_t;

localparam value_a = 1;
localparam value_b = 2;
localparam value_c = 3;

`define DUMP_V(X) initial $display(`"V: X %b %0d %0d`", X, X, $bits(X));
`define DUMP_T(X) initial begin : \dump``X \
        localparam X x = '1; \
        $display(`"T: X %0d %b`", $bits(X), x); \
    end

module mod #(
    localparam integer_t LV1 = value_a, LV2 = value_b,
    parameter type PT1 = logic,
    integer PV1 = 100,
    parameter type(PV1) PV2 = PV1 + value_c,
    parameter type PT2 = byte_t,
    shortint_t PV3 = 4,
    localparam LV3 = 5,
    type LT1 = byte_t, LT2 = shortint_t
);
    `DUMP_V(LV1) `DUMP_V(LV2) `DUMP_V(LV3)
    `DUMP_T(LT1) `DUMP_T(LT2)
    `DUMP_V(PV1) `DUMP_V(PV2) `DUMP_V(PV3)
    `DUMP_T(PT1) `DUMP_T(PT2)
endmodule

module top;
    mod m1();
    mod #(byte, 7, 10, logic [3:0], 5) m2();
    mod #(reg [8:0], 8, 11, integer, 6) m3();
endmodule
