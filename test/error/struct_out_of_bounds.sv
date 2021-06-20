// pattern: pattern index 1 is out of bounds for struct packed \{..logic x;.\}
module top;
    struct packed {
        logic x;
    } s = '{ 1: 1 };
endmodule
