`define MACRO(arg) $display(arg, `__FILE__, `__LINE__);
`define MACRO_NO_ARG $display(`__FILE__, `__LINE__);
module top;
`include "file_line.vh"
`EXTRA_CASE
initial begin
    $display(`__FILE__, `__LINE__);
    `MACRO_NO_ARG
    $display("a", `__FILE__, `__LINE__);
    $display("b", `__FILE__, `__LINE__);
    `MACRO("c")
    `MACRO("d")
    $display("e", `__FILE__, `__LINE__);
    `MACRO("f")
end
endmodule
