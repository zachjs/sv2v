// pattern: could not find modport intf1\.ModportC
`include "interface_mismatch.svh"
module Module(intf);
    Interface1.ModportC intf;
endmodule
module top;
    Interface1 intf1();
    Module m(intf1);
endmodule
