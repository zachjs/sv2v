`define DUMP(name) initial $display(`"name W=%b $bits(X)=%0d Y=%b $bits(Z)=%0d`", W, X, Y, Z);

module InterfaceA #(
    parameter W = 9,
    parameter X = 9,
    parameter [7:0] Y = 9,
    parameter Z = 9
);
    `DUMP(InterfaceA)
endmodule

module InterfaceB;
    parameter W = 9;
    parameter X = 9;
    parameter [7:0] Y = 9;
    parameter Z = 9;
    `DUMP(InterfaceB)
endmodule

module ModuleA #(
    parameter W = 9,
    parameter X = 9,
    parameter [7:0] Y = 9,
    parameter Z = 9
);
    `DUMP(ModuleA)
endmodule

module ModuleB;
    parameter W = 9;
    parameter X = 9;
    parameter [7:0] Y = 9;
    parameter Z = 9;
    `DUMP(ModuleB)
endmodule

module top;
`define PARAMS_A #(.Y(6), .X(5), .Z(7), .W(4))
`define PARAMS_B #(0, 1, 2, 3)
    InterfaceA `PARAMS_A ia1();
    InterfaceA `PARAMS_B ia2();
    InterfaceB `PARAMS_A ib1();
    InterfaceB `PARAMS_B ib2();
    ModuleA `PARAMS_A ma1();
    ModuleA `PARAMS_B ma2();
    ModuleB `PARAMS_A mb1();
    ModuleB `PARAMS_B mb2();
endmodule
