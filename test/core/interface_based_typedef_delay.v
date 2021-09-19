module poof;
    initial $display("poof");
endmodule
module alt;
    poof poof[0:3]();
endmodule
`include "interface_based_typedef.v"
