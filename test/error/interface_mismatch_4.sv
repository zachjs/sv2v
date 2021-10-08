// pattern: could not resolve modport binding intf1\.ModportC for port intf of type Interface1\.ModportB
// location: interface_mismatch_4.sv:9:5
`include "interface_mismatch.svh"
module Module(intf);
    Interface1.ModportB intf;
endmodule
module top;
    Interface1 intf1();
    Module m(intf1.ModportC);
endmodule
