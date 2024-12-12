module top;
    wire input_a, input_b;
    wire [1:0] input_c;
    wire [5:0] input_d;
    wire [1:0] output_a, output_b;
    wire [5:0] output_c, output_d;
    mod m(
        input_a, input_b, input_c, input_d,
        output_a, output_b, output_c, output_d
    );
    integer i;
    localparam bits = $bits({input_a, input_b, input_c, input_d });
    assign {input_a, input_b, input_c, input_d} = i;
    initial begin
        $monitor(
            "%03d (%b, %b, %b, %b) -> (%b, %b, %b, %b)",
            $time,
            input_a, input_b, input_c, input_d,
            output_a, output_b, output_c, output_d
        );
        repeat(3)
            for (i = 0; i < 2 ** bits; i = i + 1)
                #1;
    end
endmodule
