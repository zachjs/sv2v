`ifndef TRAIL
`define TRAIL ,
`endif

module mod #(
    parameter [23:0] KEY = "INV"
) (a, b, c);
    input wire [31:0] a, b, c;
    initial #1 $display("%s a=%0d b=%0d c=%0d", KEY, a, b, c);
endmodule

module top;
    mod #("MA0") MA0(, , );
    mod #("MA1") MA1(1, , );
    mod #("MA2") MA2(1, 2, );
    mod #("MA3") MA3(1, 2, 3);
    mod #("MA4") MA4(1, , 3);
    mod #("MA5") MA5(1, , 3);
    mod #("MA6") MA6(, 2, 3);
    mod #("MA7") MA7(, , 3);
    mod #("MB0") MB0(, , `TRAIL);
    mod #("MB1") MB1(1, , `TRAIL);
    mod #("MB2") MB2(1, 2, `TRAIL);
    mod #("MB3") MB3(1, 2, 3 `TRAIL);
    mod #("MB4") MB4(1, , 3 `TRAIL);
    mod #("MB5") MB5(1, , 3 `TRAIL);
    mod #("MB6") MB6(, 2, 3 `TRAIL);
    mod #("MB7") MB7(, , 3 `TRAIL);
endmodule
