module top;
    localparam FOO = "some useful string";
    localparam type T = type(FOO);
    localparam T BAR = "some other useful string"; // clipped
    initial $display("'%s' '%s'", FOO, BAR);
endmodule
