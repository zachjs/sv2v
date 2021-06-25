`timescale 1ns / 10ps
timeunit 100ps;
timeprecision 1ps;
module top_1;
    `timescale 1ns / 10ps
    timeunit 1ps;
    timeprecision 1ps;
endmodule
module top_2;
    timeunit 100ps / 10fs;
endmodule
module top_3;
    timeunit 10.0ps / 10fs;
endmodule
module top_4;
    timeunit 100ps;
    timeprecision 10fs;
endmodule
module top;
    initial $display("Hello!");
endmodule
