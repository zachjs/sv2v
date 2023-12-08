module top;
    localparam a = "abcd";
    localparam [63:0] b = "abcd";
    reg [3:0][7:0] c = "abcd";
    integer d = b; // truncate
    localparam [31:0] e = "abcd";
    `include "string_byte_order.vh"
endmodule
