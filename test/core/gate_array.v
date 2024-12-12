module mod(
    input input_a, input_b,
    input [1:0] input_c,
    input [5:0] input_d,
    output [1:0] output_a, output_b,
    output [5:0] output_c, output_d
);
    and gate_a[1:0] (output_a, input_a, input_c);
    and gate_b[1:0] (output_b, input_a, input_b);
    and gate_c[5:0] (output_c, input_a, input_d);
    and gate_d[5:0] (output_d, input_b, input_d);
endmodule
