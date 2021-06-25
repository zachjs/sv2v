package PkgA;
    localparam X = 1;
    localparam Y = 2;
endpackage
package PkgB;
    localparam X = 3;
    localparam Z = 4;
endpackage
import PkgA::*;
import PkgB::*;
localparam X = 5;
module top;
    initial $display(X, Y, Z);
endmodule
