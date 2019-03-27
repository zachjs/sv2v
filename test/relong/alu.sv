`default_nettype none

module ALU(
    input logic [2:0] operation,
    input logic [31:0] left, right,
    output logic [31:0] result
);

    always_comb begin
        result = 32'b0;
        case(operation)
            3'd0: begin
                // Right logical shift
                // Only need the lowest 5 bits for 32 bit input
                result = $unsigned(left) >> right[4:0];
            end
            3'd1: begin
                // Right arithmetic shift
                result = $signed(left) >>> right[4:0];
            end
            3'd2: begin
                // Signed Comparison
                result = $signed(left) < $signed(right);
            end
            3'd3: begin
                // Unsigned comparison
                result = $unsigned(left) < $unsigned(right);
            end
        endcase
    end
endmodule