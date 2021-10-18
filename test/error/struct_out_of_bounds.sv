// pattern: pattern index 1 is out of bounds for struct packed \{..logic x;.\}
// location: struct_out_of_bounds.sv:4:5
module top;
    struct packed {
        logic x;
    } s = '{ 1: 1 };
endmodule
