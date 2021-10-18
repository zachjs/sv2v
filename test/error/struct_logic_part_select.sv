// pattern: illegal access to range \[0\+:1\] of s\.x, which has type logic
// location: struct_logic_part_select.sv:7:13
module top;
    struct packed {
        logic x;
    } s;
    initial s.x[0+:1] = 1;
endmodule
