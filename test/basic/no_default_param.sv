`define DUMP(name) initial $display(`"name W=%b $bits(X)=%0d Y=%b $bits(Z)=%0d", W, $bits(X), Y, $bits(Z));

interface InterfaceA #(
    parameter W,
    parameter type X,
    parameter byte Y,
    parameter type Z
);
    `DUMP(InterfaceA)
endinterface

interface InterfaceB;
    parameter W;
    parameter type X;
    parameter byte Y;
    parameter type Z;
    `DUMP(InterfaceB)
endinterface

module ModuleA #(
    parameter W,
    parameter type X,
    parameter byte Y,
    parameter type Z
);
    `DUMP(ModuleA)
endmodule

module ModuleB;
    parameter W;
    parameter type X;
    parameter byte Y;
    parameter type Z;
    `DUMP(ModuleB)
endmodule

module top;
`define PARAMS_A #(.Y(6), .X(logic [4:0]), .Z(logic [6:0]), .W(4))
`define PARAMS_B #(0, logic, 2, logic [2:0])
    InterfaceA `PARAMS_A ia1();
    InterfaceA `PARAMS_B ia2();
    InterfaceB `PARAMS_A ib1();
    InterfaceB `PARAMS_B ib2();
    ModuleA `PARAMS_A ma1();
    ModuleA `PARAMS_B ma2();
    ModuleB `PARAMS_A mb1();
    ModuleB `PARAMS_B mb2();
endmodule
