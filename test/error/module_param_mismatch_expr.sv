// pattern: parameter "P" in instance "mod" of "Module" expects an expression, but was given type logic
module Module;
    parameter P = 0;
    logic [P-1:0] x;
endmodule
module top;
    Module #(logic) mod();
endmodule
