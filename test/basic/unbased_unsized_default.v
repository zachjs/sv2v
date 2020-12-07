module Example(inp);
    input [19:0] inp;
    initial #1 $display("%b", inp);
endmodule

module top;
    Example e1({20 {1'sb0}});
    Example e2({20 {1'sb1}});
    Example e3({20 {1'sbx}});
    Example e4({20 {1'sbz}});
endmodule
