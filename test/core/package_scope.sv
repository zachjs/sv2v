`define DUMP(key) initial $display(`"key %0d`", X);

package P;
    localparam X = 1;
endpackage

package Q;
    localparam X = 2;
endpackage

module ExampleA;
    import P::*;
    localparam X = 3;
    `DUMP(A)
endmodule

module ExampleB;
    localparam X = 3;
    import P::*;
    `DUMP(B)
endmodule

module ExampleC;
    import P::*;
    `DUMP(C)
endmodule

module ExampleD;
    import Q::*;
    `DUMP(D)
endmodule

module ExampleE;
    import P::*;
    import Q::X;
    `DUMP(E)
endmodule

module ExampleF;
    import Q::X;
    import P::*;
    `DUMP(F)
endmodule

module ExampleG;
    import P::*;
    import Q::*;
    // allowed but can't reference C
endmodule

package R;
    import P::X;
    export P::X;
endpackage

package S;
    import P::X;
    export R::X; // oof but it's allowed
endpackage

module ExampleH;
    import R::*;
    import S::*;
    `DUMP(H)
endmodule

module ExampleI;
    import R::X;
    import S::X;
    `DUMP(I)
endmodule

module ExampleJ;
    import R::*;
    `DUMP(J)
    import S::X;
endmodule

module ExampleK;
    import P::X;
    if (1) begin : blk1
        import P::X;
        `DUMP(K1)
    end
    if (1) begin : blk2
        import Q::X;
        `DUMP(K2)
    end
    if (1) begin : blk3
        localparam X = 3;
        `DUMP(K3)
    end
    if (1) begin : blk4
        import Q::*;
        `DUMP(K4)
    end
    `DUMP(K0)
endmodule

module ExampleL;
    import P::X;
    import R::X;
    `DUMP(L)
endmodule

package T;
    import P::X;
    export P::*;
endpackage

package U;
    import P::*;
    export P::X;
endpackage

package V;
    import P::*;
    export P::*;
    localparam Y = X;
endpackage

package W;
    import P::*;
    export P::*;
    task help;
        $display("W::help() %0d", X);
    endtask
endpackage

module ExampleM;
    if (1) begin : blk1
        import T::X;
        `DUMP(M1)
    end
    if (1) begin : blk2
        import U::X;
        `DUMP(M2)
    end
    if (1) begin : blk3
        import V::X;
        `DUMP(M3)
    end
    if (1) begin : blk4
        import W::X;
        `DUMP(M4)
        initial W::help;
        initial W::help();
    end
endmodule

module ExampleN;
    import P::*;
    if (1) begin : blk1
        import P::X;
        `DUMP(N1)
    end
    import Q::X;
    `DUMP(N2)
endmodule

module ExampleO;
    import P::*;
    if (1) begin : blk1
        import P::*;
        `DUMP(O1)
    end
    import Q::X;
    `DUMP(O2)
endmodule

module top;
endmodule
