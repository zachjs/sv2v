package P;
    localparam X = 1;
endpackage
package Q;
    import P::X;
    export P::*;
    localparam Y = 2;
endpackage
package R;
    import Q::X;
    export Q::*;
    localparam Z = 3;
endpackage
package S;
    import P::X;
    import Q::Y;
    import R::Z;
    export *::*;
endpackage
module top;
    import S::*;
    initial $display(X, Y, Z);
endmodule
