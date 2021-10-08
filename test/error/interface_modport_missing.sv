// pattern: can't deduce modport for interface j bound to port j, within scope top
// location: interface_modport_missing.sv:14:5
interface Interface;
    logic x;
endinterface
module Module(i, j);
    Interface i;
    logic j;
    assign i.x = j.x;
endmodule
module top;
    Interface i();
    Interface j();
    Module m(i, j);
endmodule
