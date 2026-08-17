module top;
    localparam BASE = 4;

    function [2:0] enum_next;
        input [2:0] v;
        begin
            case (v)
                3'd4: enum_next = 3'd5;
                3'd5: enum_next = 3'd6;
                3'd6: enum_next = 3'd7;
                3'd7: enum_next = 3'd4;
                default: enum_next = 3'd4;
            endcase
        end
    endfunction

    generate
        if (1) begin : blk
            reg [2:0] x;
            initial begin
                x = 3'd4;
                $display("scoped_next=%0d", enum_next(x));
            end
        end
    endgenerate
endmodule
