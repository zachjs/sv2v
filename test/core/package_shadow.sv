package P;
    localparam X = 10;
    localparam Y = X + 1;
    function integer flip;
        input integer X;
        return ~X;
    endfunction
endpackage

package Q;
    import P::*;
    localparam Y = X + 1000;
endpackage

package R;
    import Q::*;
    export Q::Y;
endpackage

package S;
    typedef enum logic { A, B } enum_t;
    function enum_t flop;
        input enum_t X;
        if (X == A) return B;
        else return A;
    endfunction
endpackage

module top;
    import P::*;
    localparam X = 20;
    localparam Z = Y + 1;
    import S::flop;
    typedef enum logic [3:0] { T = 3, U } enum_t;
    initial begin
        $display("X %0d", X);
        $display("P::Y %0d", P::Y);
        $display("Z %0d", Z);
        $display("R::Y %0d", R::Y);
        $display("flip(0) %0d", flip(0));
        $display("T %b", T);
        $display("U %b", U);
        $display("flop(0) %b", flop(0));
        $display("flop(1) %b", flop(1));
    end
endmodule
