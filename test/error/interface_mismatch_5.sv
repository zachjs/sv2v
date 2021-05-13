// pattern: could not resolve modport binding intf1\.ModportC for port intf of type Interface1\.ModportC
`include "interface_mismatch.svh"
module Module(intf);
    Interface1.ModportC intf;
endmodule
module top;
    Interface1 intf1();
    Module m(intf1.ModportC);
endmodule
