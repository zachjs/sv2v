interface Interface;
    logic x;
    modport Modport(
        input x
    );
    initial $display("Interface", x);
endinterface
module Module(Interface.Modport foo);
    initial $display("Module", foo.x);
endmodule
module top;
    Interface i();
    Module m(i);
endmodule
