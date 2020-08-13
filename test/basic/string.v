module top;
    localparam FOO = "some useful string";
    localparam BAR = "some other useful string";
    initial $display("'%s' '%s'", FOO, BAR);
endmodule
