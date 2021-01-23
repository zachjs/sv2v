// pattern: identifier "X" ambiguously refers to the definitions in any of PkgA, PkgB
package PkgA;
    localparam X = 1;
endpackage
package PkgB;
    localparam X = 3;
endpackage
import PkgA::*;
import PkgB::*;
module top;
    initial $display(X);
endmodule
