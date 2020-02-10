localparam SOME_VAL = 3;
interface Interface;
    logic x;
    modport Modport(
        input x
    );
    initial $display("Interface %d %d", x, SOME_VAL);
    generate
        for (genvar g = 10; g < 15; ++g) begin
            initial $display(g);
        end
    endgenerate
endinterface
module Module(Interface.Modport foo);
    initial $display("Module %d", foo.x);
endmodule
module top;
    generate
        for (genvar g = 0; g < 5; ++g) begin
            initial $display(g);
        end
    endgenerate
    Interface i();
    Module m(i);
endmodule
