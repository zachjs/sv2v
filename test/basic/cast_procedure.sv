`define EXPR $unsigned(WIDTH'(ONES))
`define TEST(size) \
    localparam WIDTH = ONE * size; \
    localparam x = $unsigned(WIDTH'(ONES)); \
    integer y, z; \
    localparam type T = logic [WIDTH-1:0]; \
    y = T'(ones); \
    z = $unsigned(WIDTH'(ones)); \
    $display(`"size: %b %b %b %b`", x, y, z, $unsigned(WIDTH'(ones)));

module top;
    parameter ONE = 1;
    parameter signed [0:0] ONES = 1'sb1;
    logic signed [0:0] ones;
    initial ones = 1'sb1;
    task t;
        `TEST(6)
    endtask
    function f;
        input integer unused;
        `TEST(7)
    endfunction
    initial t;
    initial begin
        integer a;
        a = f(0);
    end
    initial begin
        `TEST(8)
    end
endmodule
