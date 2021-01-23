// pattern: identifier "X" ambiguously refers to the definitions in any of P, Q
package P;
    localparam X = 1;
endpackage
package Q;
    localparam X = 2;
endpackage
module top;
    import P::*;
    import Q::*;
    initial $display(X);
endmodule
