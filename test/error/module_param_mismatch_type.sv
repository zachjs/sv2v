// pattern: parameter "P" in instance "mod" of "Module" expects a type, but was given expression 1
module Module;
    parameter type P;
    P x;
endmodule
module top;
    Module #(1) mod();
endmodule
