`include "net_or_var.vh"
module top;
    typedef struct packed {
        logic [3:0] a;
        logic [2:0] b;
    } t;

    `TEST_ALL(logic, logic)
    `TEST_ALL(wire, wire)
    `TEST_ALL(wire logic, wire_logic)
    `TEST_ALL(wand, wand)
    `TEST_ALL(wand logic, wand_logic)
    `TEST_ALL(var, var)
    `TEST_ALL(var logic, var_logic)
    `TEST_ALL(reg, reg)
    `TEST_ALL(var reg, var_reg)

    `TEST_BASE(t, t)
    `TEST_BASE(wire t, wire_t)
    `TEST_BASE(wand t, wand_t)
    `TEST_BASE(var t, var_t)
endmodule
