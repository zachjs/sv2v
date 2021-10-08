// pattern: Modport not in port list: Interface i\. Is this an interface missing a port list\?
// location: interface_modport_unlisted.sv:7:5
interface Interface;
    logic x;
endinterface
module Module;
    Interface i;
endmodule
