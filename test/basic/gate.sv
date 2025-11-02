module top;
    reg input_a, input_b, input_c;

    wire output_and, output_and_delay;
    wire output_not, output_buf_delay;
    and (output_and, input_a, input_b);
    and #1 (output_and_delay, input_a, input_b);
    not (output_not, input_a);
    buf #2 foo_name (output_buf_delay, input_a);

    wire output_bufif0_delay, output_bufif1_delay;
    wire output_notif0_delay, output_notif1_delay;
    bufif0 (output_bufif0_delay, input_a, input_b);
    bufif1 (output_bufif1_delay, input_a, input_b);
    notif0 (output_notif0_delay, input_a, input_b);
    notif1 (output_notif1_delay, input_a, input_b);

    wire output_cmos, output_rcmos;
    cmos (output_cmos, input_a, input_b, input_c);
    rcmos (output_rcmos, input_a, input_b, input_c);

    wire output_nmos, output_pmos;
    wire output_rnmos, output_rpmos;
    nmos (output_nmos, input_a, input_b);
    pmos (output_pmos, input_a, input_b);
    rnmos (output_rnmos, input_a, input_b);
    rpmos (output_rpmos, input_a, input_b);

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
            output_bufif0_delay, output_bufif1_delay,
            output_notif0_delay, output_notif1_delay,
            output_cmos, output_rcmos,
            output_nmos, output_pmos,
            output_rnmos, output_rpmos,
            output_nand, output_or, output_nor, output_xor, output_xnor);

        #1;
        #1; input_a = 1;
        #1; input_c = 0;
        #1; input_b = 0;
        #1; input_b = 1;
        #1; input_c = 1;
        #1;

        #1; input_a = 0;
        #1; input_b = 0;
        #1; input_a = 0;
        #1; input_b = 1;
        #1; input_c = 0;
        #1; input_a = 1;
        #1; input_b = 0;
        #1; input_c = 1;
        #1; input_a = 1;
        #1; input_b = 1;
        #1;
        #1;
        #1;
    end
endmodule
