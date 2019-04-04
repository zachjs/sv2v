`define CASE_A(name, dims) \
module name(clock, in, out); \
    input wire clock, in; \
    output logic dims out; \
    initial out[0] = 0; \
    initial out[1] = 0; \
    initial out[2] = 0; \
    always @(posedge clock) begin \
 \
        out[2][4] = out[2][3]; \
        out[2][3] = out[2][2]; \
        out[2][2] = out[2][1]; \
        out[2][1] = out[2][0]; \
        out[2][0] = out[1][4]; \
 \
        out[1][4] = out[1][3]; \
        out[1][3] = out[1][2]; \
        out[1][2] = out[1][1]; \
        out[1][1] = out[1][0]; \
        out[1][0] = out[0][4]; \
 \
        out[0][4] = out[0][3]; \
        out[0][3] = out[0][2]; \
        out[0][2] = out[0][1]; \
        out[0][1] = out[0][0]; \
        out[0][0] = in; \
 \
    end \
endmodule

`CASE_A(A1, [2:0][4:0])
`CASE_A(A2, [0:2][0:4])
`CASE_A(A3, [0:2][4:0])
`CASE_A(A4, [2:0][0:4])

`define CASE_B(name, dims) \
module name(clock, in, out); \
    input wire clock, in; \
    output logic dims out; \
    initial out[1] = 0; \
    initial out[2] = 0; \
    initial out[3] = 0; \
    always @(posedge clock) begin \
 \
        out[3][5] = out[3][4]; \
        out[3][4] = out[3][3]; \
        out[3][3] = out[3][2]; \
        out[3][2] = out[3][1]; \
        out[3][1] = out[2][5]; \
 \
        out[2][5] = out[2][4]; \
        out[2][4] = out[2][3]; \
        out[2][3] = out[2][2]; \
        out[2][2] = out[2][1]; \
        out[2][1] = out[1][5]; \
 \
        out[1][5] = out[1][4]; \
        out[1][4] = out[1][3]; \
        out[1][3] = out[1][2]; \
        out[1][2] = out[1][1]; \
        out[1][1] = in; \
 \
    end \
endmodule

`CASE_B(B1, [3:1][5:1])
`CASE_B(B2, [1:3][1:5])
`CASE_B(B3, [1:3][5:1])
`CASE_B(B4, [3:1][1:5])

`define CASE_C(name, dims) \
module name(clock, in, out); \
    input wire clock, in; \
    output logic dims out; \
    initial out[2] = 0; \
    initial out[3] = 0; \
    initial out[4] = 0; \
    always @(posedge clock) begin \
 \
        out[4][6] = out[4][5]; \
        out[4][5] = out[4][4]; \
        out[4][4] = out[4][3]; \
        out[4][3] = out[4][2]; \
        out[4][2] = out[3][6]; \
 \
        out[3][6] = out[3][5]; \
        out[3][5] = out[3][4]; \
        out[3][4] = out[3][3]; \
        out[3][3] = out[3][2]; \
        out[3][2] = out[2][6]; \
 \
        out[2][6] = out[2][5]; \
        out[2][5] = out[2][4]; \
        out[2][4] = out[2][3]; \
        out[2][3] = out[2][2]; \
        out[2][2] = in; \
 \
    end \
endmodule

`CASE_C(C1, [4:2][6:2])
`CASE_C(C2, [2:4][2:6])
`CASE_C(C3, [2:4][6:2])
`CASE_C(C4, [4:2][2:6])
