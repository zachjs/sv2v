module mod(
    input logic input_a, input_b,
    input logic [1:0] input_c,
    input logic [1:0][2:0] input_d,
    output logic [1:0] output_a, output_b,
    output logic [1:0][2:0] output_c,
    output logic [0:1][2:0] output_d
);
    and gate_a[1:0] (output_a, input_a, input_c);
    and gate_b[1:0] (output_b, input_a, input_b);
    and gate_c[1:0][2:0] (output_c, input_a, input_d);
    and gate_d[1:0][0:2] (output_d, input_b, input_d);
endmodule
