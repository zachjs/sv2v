// pattern: parameter "P" in instance "intf" of "Interface" expects an expression, but was given type logic
interface Interface;
    parameter P = 0;
    logic [P-1:0] x;
endinterface
module top;
    Interface #(logic) intf();
endmodule
