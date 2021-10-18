// pattern: invalid pattern key -1 is not a type, field name, or index
// location: struct_invalid_key.sv:4:5
module top;
    struct packed {
        logic x;
    } s = '{ -1: 1 };
endmodule
