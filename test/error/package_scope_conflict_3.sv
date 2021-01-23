// pattern: declaration of X conflicts with prior import of P::X
package P;
    localparam X = 1;
endpackage
module top;
    import P::X;
    localparam X = 2;
endmodule
