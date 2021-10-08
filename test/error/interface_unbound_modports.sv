// pattern: instance m of Module has unconnected interface ports: i, k
// location: interface_unbound_modports.sv:13:5
interface Interface;
    logic x;
endinterface
module Module(i, j, k);
    Interface i;
    Interface j;
    Interface k;
endmodule
module top;
    Interface j();
    Module m(.j);
endmodule
