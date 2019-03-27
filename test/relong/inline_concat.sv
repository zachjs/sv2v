`default_nettype none

module Device(
    input logic [7:0] a, b,
    output logic [7:0] result
);

    logic [7:0] result1, result2;

    OrParts helper1(.data({a, b}), .result(result1));

    logic [15:0] bothInputs;
    assign bothInputs = {a, b};
    OrParts helper2(.data(bothInputs), .result(result2));

    // Expect both result1 and result2 to be equal so...
    assign result = result1 & result2;

endmodule

module OrParts(
    input logic [15:0] data,
    output logic [7:0] result
);

    always_comb begin
        result = data[15:8] | data[7:0];
    end

endmodule
