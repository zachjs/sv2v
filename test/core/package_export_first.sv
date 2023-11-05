package P;
    localparam X = 1;
endpackage
package Q;
    export P::X;
    import P::X;
endpackage
module top;
    initial $display(Q::X);
endmodule
