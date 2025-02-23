module M(x, y);
    parameter P = 0;
    localparam L = 10;
    function automatic integer F;
        input integer inp;
        return inp + L;
    endfunction
    input wire integer x = F(P); // defining a default here is non-standard
    input wire integer y;
    initial #1 $display("M %0d %0d", x, y);
endmodule

module N #(
    parameter type P,
    parameter type Q = integer,
    localparam L = 10
) (
    input wire P x = $bits(P) + L,
    input wire Q y = $bits(Q) + L
);
    initial #1 $display("N %b %b", x, y);
endmodule

interface I #(
    parameter type P,
    parameter type Q = integer,
    localparam L = 10
) (
    input wire P x = $bits(P) + L,
    input wire Q y = $bits(Q) + L
);
    initial #2 $display("I %b %b", x, y);
endinterface

module top;
    M m0(0, 0);
    M m1(.y(1));
    M#(1) m2(.y(2));

    N#(logic) n0();
    N#(logic) n1(1'b0);
    N#(logic) n2(.y(2));
    N#(byte) n3();
    N#(byte, shortint) n4();

    I#(logic) i0();
    I#(logic) i1(0);
    I#(logic) i2(.y(2));
    I#(byte) i3();
    I#(byte, shortint) i4();
endmodule
