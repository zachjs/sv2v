`default_nettype none

module ALU(
    input wire [2:0] operation,
    input wire [31:0] left, right,
    output reg [31:0] result
);

    // 1. Unsigned should be a no-op so simply remove those function calls
    // 2. Signed modifiers don't appear to be supported on VTR, so they might need to be expanded with logic for actually doing the signed operation.
    // 3. Likewise, the triple shift (arithmetic shift) operator doesn't appear to be supported in VTR so it also might need to be expanded manually.
    always @* begin
        result = 32'b0;
        case(operation)
            3'd0: begin
                // Right logical shift
                // Only need the lowest 5 bits for 32 bit input
                result = left >> right[4:0];
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
                result = left < right;
            end
        endcase
    end

    // Extra stuff for example
    reg [31:0] result2;
    reg [31:0] __temp;
    // sameResult: assert property (result == result2);
    always @* begin
        result2 = 32'b0;
        __temp = 32'b0;
        case(operation)
            3'd0: begin
                // Right logical shift
                // Only need the lowest 5 bits for 32 bit input
                result2 = left >> right[4:0];
            end
            3'd1: begin
                // Right arithmetic shift

                // 1. Take the MSB and replicate it to form a mask
                // 2. Create a mask for the bits that the shift will set correctly and mask them off of the MSB replication (sets 32 - shift bits correctly)
                // 3. Combine the shift part and the masked part
                __temp = {32{left[31]}} & ~ ((33'd1 << (6'd32 - right[4:0])) - 33'd1);
                result2 = __temp | (left >> right[4:0]);
            end
            3'd2: begin
                // Signed Comparison
                case({left[31], right[31]})
                    2'b00: result2 = left < right; // Both positive
                    2'b01: result2 = 32'd0; // + < - (always false)
                    2'b10: result2 = 32'd1; // - < + (always true)
                    2'b11: result2 = -left > -right; // Both negative
                endcase
            end
            3'd3: begin
                // Unsigned comparison
                result2 = left < right;
            end
        endcase
    end

endmodule