// pattern: could not find "X" in package "Q"
package P;
    localparam X = 1;
endpackage
package Q;
    import P::*;
    export P::*;
    localparam Y = P::X;
endpackage
module top;
    initial $display(Q::X);
endmodule
