interface InterfaceA;
    logic x;
    modport M(input x);
endinterface

interface InterfaceB;
    logic y;
    modport N(input y);
endinterface

module ModuleA(modport_a);
    InterfaceA modport_a;
    initial $display("ModuleA %b", modport_a.x);
endmodule

module ModuleB(modport_b);
    InterfaceB modport_b;
    InterfaceA interface_a();
    ModuleA module_a(interface_a.M);
    initial $display("ModuleB %b", modport_b.y);
    assign interface_a.x = 0;
endmodule

module ModuleC;
    InterfaceB interface_b();
    ModuleB module_b(interface_b.N);
    assign interface_b.y = 1;
endmodule

module top;
    ModuleC module_c();
endmodule
