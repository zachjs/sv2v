// pattern: import of P::X conflicts with prior import of Q::X
package P;
    localparam X = 1;
endpackage
package Q;
    localparam X = 2;
endpackage
module top;
    import Q::*;
    initial $display(X); // imports Q::X
    import P::X; // illegal
endmodule
