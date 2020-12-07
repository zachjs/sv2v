module Example(inp);
    input [4][5] inp;
    initial #1 $display("%b", inp);
endmodule

module top;
    Example e1('{default:'0});
    Example e2('{default:'1});
    Example e3('{default:'x});
    Example e4('{default:'z});
endmodule
