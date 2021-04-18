// pattern: inlining instance "intf" of interface "Interface" would make expression "x" used in "intf" resolvable when it wasn't previously
interface Interface;
    assign x = 1;
endinterface
module top;
    wire x;
    Interface intf();
endmodule
