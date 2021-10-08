// pattern: inlining instance "intf" of interface "Interface" would make expression "x" used in "intf" resolvable when it wasn't previously
// location: interface_bad_expr.sv:4:5
interface Interface;
    assign x = 1;
endinterface
module top;
    wire x;
    Interface intf();
endmodule
