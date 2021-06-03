package P;
    typedef logic T;
endpackage

package Q;
    typedef logic [7:0] U;
endpackage

`define DUMP(x) initial $display(`"x: %0d %b`", $bits(x), x);

module top;
    P::T a;
    import Q::U;
    typedef U T;
    U b;
    T c;
    `DUMP(a)
    `DUMP(b)
    `DUMP(c)
endmodule
