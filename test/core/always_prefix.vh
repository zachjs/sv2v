module mod(
    input wire [3:0] idx,
    input wire [14:0] data
);
    localparam Y = 2;
    localparam X = 10000;

`define TEST(expr, trigger, extra) \
    if (1) begin \
        function automatic f; \
            input reg ignored; \
            localparam X = Y + 1; \
            localparam THREE = X; \
            f = expr; \
        endfunction \
        `ALWAYS(trigger) begin : blk \
            localparam ZERO = 0; \
            $display(`"%2d %b expr trigger`", \
                $time, f(ZERO) extra); \
        end \
    end

`define TEST_SIMPLE(expr) `TEST(expr, expr, )

    `TEST_SIMPLE(data)
    `TEST_SIMPLE(data[1])
    `TEST_SIMPLE(data[4])
    `TEST_SIMPLE(data[4:1])
    `TEST_SIMPLE(data[10:1])

localparam ONE = 1;
parameter FOUR = 4;
    `TEST_SIMPLE(data[ONE])
    `TEST_SIMPLE(data[FOUR])
    `TEST_SIMPLE(data[FOUR:ONE])

    `TEST(data[idx], data or idx, )
    `TEST(data[idx+:2], data or idx, )

    `TEST(data[THREE], data[3], )
    `TEST(data[ignored], data, )
    `TEST(data[THREE], data[0] or data[3], & data[0])
endmodule
