`define DUMP_V(X) initial $display(`"V: X %b %0d %0d`", X, X, $bits(X));
`define DUMP_T(X) initial begin : \dump``X \
        localparam [X - 1:0] x = 1'sb1; \
        $display(`"T: X %0d %b`", X, x); \
    end

module mod;
    localparam integer LV1 = 1, LV2 = 2;
    parameter PT1 = 1;
    parameter integer PV1 = 100;
    parameter integer PV2 = PV1 + 3;
    parameter PT2 = 8;
    parameter [15:0] PV3 = 4;
    localparam LV3 = 5;
    localparam LT1 = 8, LT2 = 16;
    `DUMP_V(LV1) `DUMP_V(LV2) `DUMP_V(LV3)
    `DUMP_T(LT1) `DUMP_T(LT2)
    `DUMP_V(PV1) `DUMP_V(PV2) `DUMP_V(PV3)
    `DUMP_T(PT1) `DUMP_T(PT2)
endmodule

module top;
    mod m1();
    mod #(8, 7, 10, 4, 5) m2();
    mod #(9, 8, 11, 32, 6) m3();
endmodule
