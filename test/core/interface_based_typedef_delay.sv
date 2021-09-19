module poof;
    initial $display("poof");
endmodule
module alt;
    poof poof[2][2]();
endmodule
`include "interface_based_typedef.sv"
