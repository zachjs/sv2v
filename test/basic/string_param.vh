`define COUNT_ONES(expr) (0 \
        + ((expr) >>  0 & 1'b1) + ((expr) >>  1 & 1'b1) + ((expr) >>  2 & 1'b1) + ((expr) >>  3 & 1'b1) \
        + ((expr) >>  4 & 1'b1) + ((expr) >>  5 & 1'b1) + ((expr) >>  6 & 1'b1) + ((expr) >>  7 & 1'b1) \
        + ((expr) >>  8 & 1'b1) + ((expr) >>  9 & 1'b1) + ((expr) >> 10 & 1'b1) + ((expr) >> 11 & 1'b1) \
        + ((expr) >> 12 & 1'b1) + ((expr) >> 13 & 1'b1) + ((expr) >> 14 & 1'b1) + ((expr) >> 15 & 1'b1) \
        + ((expr) >> 16 & 1'b1) + ((expr) >> 17 & 1'b1) + ((expr) >> 18 & 1'b1) + ((expr) >> 19 & 1'b1) \
        + ((expr) >> 20 & 1'b1) + ((expr) >> 21 & 1'b1) + ((expr) >> 22 & 1'b1) + ((expr) >> 23 & 1'b1) \
        + ((expr) >> 24 & 1'b1) + ((expr) >> 25 & 1'b1) + ((expr) >> 26 & 1'b1) + ((expr) >> 27 & 1'b1) \
        + ((expr) >> 28 & 1'b1) + ((expr) >> 29 & 1'b1) + ((expr) >> 30 & 1'b1) + ((expr) >> 31 & 1'b1) \
    )

module top;
    reg [31:0] data;

`define TEST(idx, pattern, in_width, out_width) \
    localparam p``idx = pattern; \
    wire [in_width - 1:0] i``idx; \
    wire [out_width - 1:0] o``idx; \
    assign i``idx = data[0+:in_width]; \
    Example #(p``idx, in_width) e``idx(i``idx, o``idx);

    `TEST(1, 5'b10101, 5, 3)
    `TEST(2, 10'b1110001111, 10, 7)

    integer i;
    initial begin
        data = 0;
        for (i = 0; i < 100; i = i + 1) begin
            data = 1664525 * data + 1013904223;
            #1 $display("%b %b %b", data, o1, o2);
        end
    end
endmodule
