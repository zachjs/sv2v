// pattern: unknown binding "R" specified for parameters in class specialization of "example"
class example #(
    parameter P = 1,
    parameter Q = 1
);
    typedef logic [P * Q:0] T;
endclass
module top;
    example#(.P(1), .R(2))::T x;
endmodule
