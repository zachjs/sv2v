// pattern: too many bindings specified for parameters in class specialization of "example": 3 specified \(1, 2, 3\), but only 2 available \("P", "Q"\)
class example #(
    parameter P = 1,
    parameter Q = 1
);
    typedef logic [P * Q:0] T;
endclass
module top;
    example#(1, 2, 3)::T x;
endmodule
