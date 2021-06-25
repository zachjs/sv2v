module Module_Size1(out);
    output wire out;
    assign out = 1'b1;
endmodule

module Module_Size2(out);
    output wire [1:0] out;
    assign out = 2'b11;
endmodule

module top;
    wire w;
    wire [0:0] b;

    wire o0, o1;
    wire [1:0] o2;
    Module_Size1 m0(o0);
    Module_Size1 m1(o1);
    Module_Size2 m2(o2);
endmodule
