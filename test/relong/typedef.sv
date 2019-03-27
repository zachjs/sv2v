`default_nettype none

typedef logic [31:0] Word_t;

module Example(
    input logic [3:0] data_in,
    output logic [31:0] data_out
);

    Word_t word;
    always_comb begin
        // This is the repeat operator
        word = {8{data_in}};
    end

    assign data_out = word;

endmodule



