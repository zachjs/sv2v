module top;
    localparam [2:0] foo_pkg_AccessAck = 3'd0;
    wire [2:0] test;
    assign test = foo_pkg_AccessAck;
    initial $display(test);
endmodule
