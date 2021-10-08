// pattern: port intf has type Interface1, but the binding intf2\.ModportA has type Interface2\.ModportA
// location: interface_mismatch_2.sv:9:5
`include "interface_mismatch.svh"
module Module(intf);
    Interface1 intf;
endmodule
module top;
    Interface2 intf2();
    Module m(intf2.ModportA);
endmodule
