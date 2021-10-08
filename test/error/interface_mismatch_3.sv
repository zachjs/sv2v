// pattern: port intf has type Interface1\.ModportB, but the binding intf1\.ModportA has type Interface1\.ModportA
// location: interface_mismatch_3.sv:9:5
`include "interface_mismatch.svh"
module Module(intf);
    Interface1.ModportB intf;
endmodule
module top;
    Interface1 intf1();
    Module m(intf1.ModportA);
endmodule
