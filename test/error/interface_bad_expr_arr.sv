// pattern: inlining instance "mod" of module "Module" would make expression "a\[0\]\.x" used in "mod" resolvable when it wasn't previously
// location: interface_bad_expr_arr.sv:10:5
interface InterfaceA;
    logic x;
endinterface
interface InterfaceB;
    logic x;
endinterface
module Module(InterfaceB b);
    assign a[0].x = 1;
endmodule
module top;
    InterfaceA a[1]();
    InterfaceB b();
    Module mod(b);
endmodule
