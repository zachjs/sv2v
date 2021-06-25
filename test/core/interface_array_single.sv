`define SHADOW \
    integer i; \
    Interface intfs[1:0]();

interface Interface;
    integer x;
    modport ModportA(input .x(x + 1));
    modport ModportB(input .x(x + 2));
endinterface

module ModuleA(intf);
    Interface intf;
    `SHADOW
    initial #1 $display("ModuleA got %0d", intf.x);
    ModuleN n(intf);
endmodule

module ModuleB(intf);
    Interface.ModportA intf;
    `SHADOW
    initial #1 $display("ModuleB got %0d", intf.x);
    ModuleN n(intf);
endmodule

module ModuleC(intf);
    Interface.ModportB intf;
    `SHADOW
    initial #1 $display("ModuleC got %0d", intf.x);
    ModuleN n(intf);
endmodule

module ModuleN(intf);
    Interface intf;
    `SHADOW
    initial #1 $display("ModuleN got %0d", intf.x);

    typedef struct packed {
        logic a, b;
    } Struct;
    Struct [1:0] structs;
    assign structs[1].a = structs[0].b;
    assign structs[0].a = structs[1].b;
endmodule

module top;
    Interface intfs[4:8]();
    `define LOOP for (genvar i = 4; i <= 8; ++i)
    `LOOP initial intfs[i].x = i ** 2;
    `LOOP ModuleA a1(intfs[i]);
    `LOOP ModuleA a2(intfs[i].ModportA);
    `LOOP ModuleA a3(intfs[i].ModportB);
    `LOOP ModuleB b1(intfs[i]);
    `LOOP ModuleB b2(intfs[i].ModportA);
    `LOOP ModuleC c1(intfs[i]);
    `LOOP ModuleC c2(intfs[i].ModportB);
endmodule
