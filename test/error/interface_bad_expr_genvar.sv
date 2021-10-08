// pattern: inlining instance "intf" of interface "Interface" would make expression "x" used in "intf" resolvable when it wasn't previously
// location: interface_bad_expr_genvar.sv:4:5
interface Interface;
    assign x = 1;
endinterface
module top;
    for (genvar x = 0; x < 2; x++)
        Interface intf();
endmodule
