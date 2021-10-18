// pattern: pattern '\{..x: 1,..y: 2.\} has extra named fields \["y"\] that are not in struct packed \{..logic x;.\}
// location: struct_extra_named_field.sv:4:5
module top;
    struct packed {
        logic x;
    } x = '{ x: 1, y: 2 };
endmodule
