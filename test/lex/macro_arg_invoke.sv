module top; endmodule
`define MODULE(str) module str; initial $display(`"hello str`"); endmodule
`define MACRO_A(inv, str) `inv(str)
`define MACRO_B(inv, str) ```inv(str)
`define MACRO_C(inv1, inv2, str) `inv1``inv2(str)
`MODULE(example1)
`MACRO_A(MODULE, example2)
`MACRO_B(MODULE, example3)
`MACRO_C(MOD, ULE, example4)
`MACRO_C(M, ODULE, example5)
`MACRO_C(, MODULE, example6)
