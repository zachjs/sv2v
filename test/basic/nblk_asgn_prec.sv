`define TEST(stmt) \
    begin \
        stmt; \
        #1 $display("%b", foo); \
        stmt; \
        #1 $display("%b", foo); \
    end

module top;
    reg foo;
    localparam bar = 10;
    initial begin
        `TEST(foo <= bar <= 11)
        `TEST(foo <= bar <= 11 <= 0)
        `TEST(foo <= bar <= 11 <= 0 <= 0)
    end
endmodule
