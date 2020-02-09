module Module(input wire x);
    initial $display("Module", x);
endmodule
module top;
    wire i_x;
    initial $display("Interface", i_x);
    Module m(.x(i_x));
endmodule
