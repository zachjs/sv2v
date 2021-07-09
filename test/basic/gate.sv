module top;
    reg input_a;
    reg input_b;

    wire output_and;
    wire output_and_delay;
    wire output_not;
    wire output_buf_delay;

    and (output_and, input_a, input_b);
    and #1 (output_and_delay, input_a, input_b);
    not (output_not, input_a);
    buf #2 foo_name (output_buf_delay, input_a);

    wire output_nand, output_or, output_nor, output_xor, output_xnor;
    nand (output_nand, input_a, input_b);
    or (output_or, input_a, input_b);
    nor (output_nor, input_a, input_b);
    xor (output_xor, input_a, input_b);
    xnor (output_xnor, input_a, input_b);

    initial repeat(2) begin
        $monitor("%3d ", $time,
            input_a, input_b,
            output_and, output_and_delay,
            output_not, output_buf_delay,
            output_nand, output_or, output_nor, output_xor, output_xnor);

        #1;
        #1; input_a = 1;
        #1; input_b = 0;
        #1; input_b = 1;
        #1;

        #1; input_a = 0;
        #1; input_b = 0;
        #1; input_a = 0;
        #1; input_b = 1;
        #1; input_a = 1;
        #1; input_b = 0;
        #1; input_a = 1;
        #1; input_b = 1;
        #1;
        #1;
        #1;
    end
endmodule
