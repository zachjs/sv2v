// pattern: pattern '\{..1,..2.\} doesn't have the same number of items as struct packed \{..logic x;.\}
// location: struct_extra_unnamed_field.sv:4:5
module top;
    struct packed {
        logic x;
    } x = '{ 1, 2 };
endmodule
