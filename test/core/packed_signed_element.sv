module driver #(parameter integer VALUE = 0) (
    output logic signed [3:0] signed_output,
    output logic [7:0] parameter_output
);
    assign signed_output = -1;
    assign parameter_output = VALUE;
endmodule

module top;
    typedef logic signed [3:0] element_t;
    typedef logic signed [7:0] wide_element_t;
    typedef logic signed [1:0][3:0] two_dimensional_t;
    typedef logic signed one_bit_t;
    typedef element_t unpacked_array_t [2];

    element_t [1:0] array;
    element_t [1:0][1:0] array_2d = '1;
    two_dimensional_t [1:0] array_of_2d = '1;
    one_bit_t [3:0] array_of_bits = '1;
    logic signed [1:0][3:0] signed_vector = '1;
    struct packed signed { logic [3:0] field; } [1:0] struct_array = '1;
    logic signed [3:0] unpacked_array [2] = '{-1, -1};
    localparam unpacked_array_t UNPACKED_PARAMETER = '{-1, -1};
    wide_element_t [1:0] accumulator = '0;
    var type(array[0]) type_of_element = -1;
    var type(array_of_bits[0]) type_of_bit = '1;
    var type(array_of_2d[1]) type_of_2d_element = '1;

    element_t [1:0] port_connection;
    logic [7:0] parameter_override;
    driver #(UNPACKED_PARAMETER[0])
        driver_instance(port_connection[0], parameter_override);

    task automatic copy(output element_t destination, input element_t source);
        destination = source;
    endtask

    initial begin
        array[0] = -1;
        copy(array[1], $signed(signed_vector[0]));
        accumulator[0] += array[0];
        #1;
        $display("array: %0d %0d %0d", array[0], array[1], array);
        $display("array_2d: %0d %0d", array_2d[1][0], array_2d[1]);
        $display("array_of_2d: %0d %0d", array_of_2d[1][0], array_of_2d[1]);
        $display("array_of_bits: %0d %0d", array_of_bits[0], array_of_bits);
        $display("signed_vector: %0d", signed_vector[0]);
        $display("struct_array: %0d", struct_array[0]);
        $display("unpacked_array: %0d", unpacked_array[0]);
        $display("UNPACKED_PARAMETER: %0d", UNPACKED_PARAMETER[0]);
        $display("accumulator: %0d", accumulator[0]);
        $display("type_of_element: %0d", type_of_element);
        $display("type_of_bit: %0d", type_of_bit);
        $display("type_of_2d_element: %0d", type_of_2d_element);
        $display("port_connection: %0d", port_connection[0]);
        $display("parameter_override: %0d", parameter_override);
    end
endmodule
