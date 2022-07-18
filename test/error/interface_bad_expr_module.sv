// pattern: inlining instance "mod" of module "Module" would make expression "x" used in "mod" resolvable when it wasn't previously
// location: interface_bad_expr_module.sv:6:5
interface Interface;
endinterface
module Module(Interface i);
    assign x = 1;
endmodule
module top;
    wire x;
    Interface intf();
    Module mod(intf);
endmodule
