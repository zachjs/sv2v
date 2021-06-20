// pattern: pattern index 1'bx is not an integer
module top;
    struct packed {
        logic x;
    } s = '{ 1'bx: 1 };
endmodule
