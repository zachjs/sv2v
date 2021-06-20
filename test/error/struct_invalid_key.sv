// pattern: invalid pattern key -1 is not a type, field name, or index
module top;
    struct packed {
        logic x;
    } s = '{ -1: 1 };
endmodule
