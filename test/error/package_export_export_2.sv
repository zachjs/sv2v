// pattern: could not find "X" in package "Q"
package P;
    localparam X = 1;
    localparam Y = 2;
endpackage
package Q;
    import P::*;
    export *::*;
    localparam Z = P::Y;
endpackage
module top;
    initial $display(Q::X);
endmodule
