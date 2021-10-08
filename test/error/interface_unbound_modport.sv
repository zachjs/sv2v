// pattern: instance m of Module has unconnected interface ports: i
// location: interface_unbound_modport.sv:11:5
interface Interface;
    logic x;
endinterface
module Module(i);
    Interface i;
endmodule
module top;
    Interface i();
    Module m();
endmodule
