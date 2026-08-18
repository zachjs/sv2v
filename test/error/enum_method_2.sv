// pattern: unexpected arguments \(0, 1\) passed to enum method x.next
// location: enum_method_2.sv:4:5
module top;
    enum {X} x = x.next(0, 1);
endmodule
