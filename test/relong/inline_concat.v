`default_nettype none

module Device(
    input wire [7:0] a, b,
    output wire [7:0] result
);

    wire [7:0] result1, result2;

    OrParts helper1(.data({a, b}), .result(result1));

    wire [15:0] bothInputs;
    assign bothInputs = {a, b};
    OrParts helper2(.data(bothInputs), .result(result2));

    // Expect both result1 and result2 to be equal so...
    assign result = result1 & result2;

endmodule

module OrParts(
    input wire [15:0] data,
    output reg [7:0] result
);

    // Update the module input definition since it is assigned in an always block
    always @* begin
        result = data[15:8] | data[7:0];
    end

endmodule
