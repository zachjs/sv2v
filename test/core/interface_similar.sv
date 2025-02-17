interface Interface;
    parameter P;
    logic [P - 1:0] x;
endinterface
module Module1(Interface i);
    localparam P = i.P;
    initial $display("Module1 P=%0d", P);
endmodule
module Module2(Interface i);
    Interface #(i.P) i();
    Module1 m(i);
endmodule
module top;
    Interface #(5) i();
    Module2 m(i);
endmodule
