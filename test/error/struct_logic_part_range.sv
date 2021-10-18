// pattern: illegal access to range \[0:0\] of s\.x, which has type logic
// location: struct_logic_part_range.sv:7:13
module top;
    struct packed {
        logic x;
    } s;
    initial s.x[0:0] = 1;
endmodule
