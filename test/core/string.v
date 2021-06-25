module top;
    localparam FOO = "some useful string";
    localparam WIDTH = $bits("some useful string");
    localparam [WIDTH-1:0] BAR = "some other useful string"; // clipped
    initial $display("'%s' '%s'", FOO, BAR);
endmodule
