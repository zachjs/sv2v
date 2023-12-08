module top;
    localparam a = "abcd";
    localparam b = 64'("abcd");
    logic [3:0][7:0] c = "abcd";
    integer d = b; // truncate
    localparam e = 32'("abcd");
    `include "string_byte_order.vh"
endmodule
