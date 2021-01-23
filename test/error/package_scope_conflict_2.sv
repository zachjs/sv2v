// pattern: import of Q::X conflicts with prior import of P::X
package P;
    localparam X = 1;
endpackage
package Q;
    localparam X = 2;
endpackage
module top;
    import P::X;
    import Q::X;
endmodule
