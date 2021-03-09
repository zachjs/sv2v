// pattern: couldn't find field 'y' from struct definition struct packed \{..logic x;..logic y;.\} in struct pattern '\{..x: 1.\}
module top;
    struct packed {
        logic x, y;
    } s = '{ x: 1 };
endmodule
