`define TEST(size) \
    localparam WIDTH = ONE * size; \
    localparam [WIDTH-1:0] short = ONES; \
    integer long; \
    long = short; \
    $display(`"size: %b %b %b %b`", short, long, long, short);

module top;
    parameter ONE = 1;
    parameter signed [0:0] ONES = 1'sb1;
    reg signed [0:0] ones;
    initial ones = 1'sb1;
    task t;
        begin : blk1
            `TEST(6)
        end
    endtask
    function f;
        input integer unused;
        begin : blk2
            `TEST(7)
        end
    endfunction
    initial t;
    initial begin : blk3
        integer a;
        a = f(0);
    end
    initial begin : blk4
        `TEST(8)
    end
endmodule
