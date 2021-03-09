// pattern: pattern '\{..1,..2.\} doesn't have the same number of items as struct packed \{..logic x;.\}
module top;
    struct packed {
        logic x;
    } x = '{ 1, 2 };
endmodule
