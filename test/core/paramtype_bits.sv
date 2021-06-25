module Module(out);
    parameter type T = logic;
    output T out;
    assign out = '1;
endmodule

module top;
    logic w;
    logic [$bits(w)-1:0] b;

    logic o0, o1;
    logic [1:0] o2;
    Module                          m0(o0);
    Module #(logic[$bits(w)-1:0])   m1(o1);
    Module #(logic[$bits(b)*2-1:0]) m2(o2);
endmodule
