// pattern: import of P::X conflicts with prior declaration of X
package P;
    localparam X = 1;
endpackage
module top;
    localparam X = 2;
    import P::X;
endmodule
