package P;
    localparam FOO = 1;
    localparam BAR = 2;
endpackage
import P::*;
module top;
    initial $display(FOO);
endmodule
