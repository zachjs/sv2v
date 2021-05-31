// pattern: parameter "P" in instance "intf" of "Interface" expects a type, but was given expression 1
interface Interface;
    parameter type P;
    P x;
endinterface
module top;
    Interface #(1) intf();
endmodule
