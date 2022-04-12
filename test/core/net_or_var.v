`include "net_or_var.vh"
module top;
    `TEST_ALL(reg, logic)
    `TEST_ALL(wire, wire)
    `TEST_ALL(wire, wire_logic)
    `TEST_ALL(wand, wand)
    `TEST_ALL(wand, wand_logic)
    `TEST_ALL(reg, var)
    `TEST_ALL(reg, var_logic)
    `TEST_ALL(reg, reg)
    `TEST_ALL(reg, var_reg)

    `TEST_BASE(reg [6:0], t)
    `TEST_BASE(wire [6:0], wire_t)
    `TEST_BASE(wand [6:0], wand_t)
    `TEST_BASE(reg [6:0], var_t)
endmodule
