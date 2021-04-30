// pattern: too many bindings specified for parameters in class specialization of "example"
class example #(
    parameter P = 1,
    parameter Q = 1
);
    typedef logic [P * Q:0] T;
endclass
module top;
    example#(1, 2, 3)::T x;
endmodule
