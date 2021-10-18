// pattern: pattern index 1'bx is not an integer
// location: struct_non_integer.sv:4:5
module top;
    struct packed {
        logic x;
    } s = '{ 1'bx: 1 };
endmodule
