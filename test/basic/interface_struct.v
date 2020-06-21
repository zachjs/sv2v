module top;
    wire [3:0] i_x;
    reg [1:0] i_w;
    assign i_x = 4'b1001;
    initial i_w = 2'b10;
    initial begin
        $display("%b", i_x[3]);
        $display("%b", i_x[2]);
        $display("%b", i_x[1]);
        $display("%b", i_x[0]);
    end
endmodule
