`include "no_newline.vh"
module top;
`ifdef A
    initial $display("A is defined!");
`endif
endmodule
