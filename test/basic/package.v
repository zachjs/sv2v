module top;
    localparam A_FOO = 37;
    localparam A_BAR = 97;
    localparam B_FOO = -37;
    localparam B_BAR = -97;
    localparam FOO = 37;
    localparam BAR = -97;
    initial begin
        $display(A_FOO);
        $display(A_BAR);
        $display(B_FOO);
        $display(B_BAR);
        $display(FOO);
        $display(BAR);
    end
endmodule
