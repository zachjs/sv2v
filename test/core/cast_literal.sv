`define TEST(size, literal) \
    $display(`"size'(literal) = %b`", size'(literal));
module top;
    initial begin
        `include "cast_literal.vh"
    end
endmodule
