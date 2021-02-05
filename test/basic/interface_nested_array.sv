interface Interface;
    logic x;
endinterface

module ModuleC(Interface intf, input y);
    initial $display("ModuleC %b %b", intf.x, y);
endmodule

module ModuleB(Interface intf, input y);
    initial $display("ModuleB %b %b", intf.x, y);
    ModuleC m[2:0] (intf, y);
endmodule

module ModuleA(Interface intf, input y);
    initial $display("ModuleA %b %b", intf.x, y);
    ModuleB m[2:0] (intf, y);
endmodule

module top;
    logic y;
    Interface intf();
    ModuleA m[2:0] (intf, y);
endmodule
