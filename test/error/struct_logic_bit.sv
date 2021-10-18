// pattern: illegal access to bit 0 of s\.x, which has type logic
// location: struct_logic_bit.sv:7:13
module top;
    struct packed {
        logic x;
    } s;
    initial s.x[0] = 1;
endmodule
