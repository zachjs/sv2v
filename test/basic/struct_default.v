module top;
    reg [1:0] foo = {1'b1, 1'b0};
    initial $display(foo, foo[1], foo[0]);
endmodule
