module driver #(parameter integer VALUE = 0) (
    output wire signed [3:0] signed_output,
    output wire [7:0] parameter_output
);
    assign signed_output = -1;
    assign parameter_output = VALUE;
endmodule

module top;
    reg [7:0] array;
    reg [15:0] array_2d = -1;
    reg [15:0] array_of_2d = -1;
    reg [3:0] array_of_bits = -1;
    reg signed [7:0] signed_vector = -1;
    reg [7:0] struct_array = -1;
    reg [7:0] unpacked_array = -1;
    localparam [7:0] UNPACKED_PARAMETER = -1;
    reg [15:0] accumulator = 0;
    reg signed [3:0] type_of_element = -1;
    reg signed type_of_bit = -1;
    reg signed [7:0] type_of_2d_element = -1;

    wire [7:0] port_connection;
    wire [7:0] parameter_override;
    driver #($signed(UNPACKED_PARAMETER[3:0]))
        driver_instance(port_connection[3:0], parameter_override);

    task automatic copy(
        output reg signed [3:0] destination,
        input reg signed [3:0] source
    );
        destination = source;
    endtask

    initial begin
        array[3:0] = -1;
        copy(array[7:4], $signed(signed_vector[3:0]));
        accumulator[7:0] = $signed(accumulator[7:0]) + $signed(array[3:0]);
        #1;
        $display("array: %0d %0d %0d",
            $signed(array[3:0]), $signed(array[7:4]), array);
        $display("array_2d: %0d %0d", $signed(array_2d[11:8]), array_2d[15:8]);
        $display("array_of_2d: %0d %0d",
            array_of_2d[11:8], $signed(array_of_2d[15:8]));
        $display("array_of_bits: %0d %0d",
            $signed(array_of_bits[0]), array_of_bits);
        $display("signed_vector: %0d", signed_vector[3:0]);
        $display("struct_array: %0d", $signed(struct_array[3:0]));
        $display("unpacked_array: %0d", $signed(unpacked_array[3:0]));
        $display("UNPACKED_PARAMETER: %0d", $signed(UNPACKED_PARAMETER[3:0]));
        $display("accumulator: %0d", $signed(accumulator[7:0]));
        $display("type_of_element: %0d", type_of_element);
        $display("type_of_bit: %0d", type_of_bit);
        $display("type_of_2d_element: %0d", type_of_2d_element);
        $display("port_connection: %0d", $signed(port_connection[3:0]));
        $display("parameter_override: %0d", parameter_override);
    end
endmodule
