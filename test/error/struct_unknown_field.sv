// pattern: field 'y' not found in struct packed \{..logic x;.\}
// location: struct_unknown_field.sv:8:5
module top;
    struct packed {
        logic x;
    } x;
    assign x.x = 1;
    assign x.y = 0;
endmodule
