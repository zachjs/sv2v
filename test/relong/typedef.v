`default_nettype none

// Verilog unwraps the typedef

module Example(
    input wire [3:0] data_in,
    output wire [31:0] data_out
);

    reg [31:0] word;
    always @* begin
        // This is the repeat operator
        word = {8{data_in}};
    end

    assign data_out = word;

endmodule



