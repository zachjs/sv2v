`default_nettype none

module Example(
    input wire [3:0] a,
    output wire [31:0] all
);

    function [1:0] __truncate_to_2_bits(input [1:0] i);
        __truncate_to_2_bits = i;
    endfunction

    generate
        genvar i;
        for(i = 0; i < 4; i = i + 1) begin : __gen_loop
            reg [7:0] s;
            always @* begin
                s = {__truncate_to_2_bits(3-i), a, __truncate_to_2_bits(i)};
                // s = '{
                //     first: i,
                //     middle: a,
                //     last: 3 - i
                // };
            end
            assign all[i*8+:8] = s;
        end
    endgenerate

endmodule