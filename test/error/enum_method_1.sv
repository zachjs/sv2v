// pattern: unexpected argument 0 passed to enum method x.first
// location: enum_method_1.sv:4:5
module top;
    enum {X} x = x.first(0);
endmodule
