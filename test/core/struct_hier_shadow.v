module top;
    wire [7:0] top;
    wire [2:0] x;
    assign x = 0;
    assign top = 8'hFF;
    initial begin
        #1;
        $display("%b %b", x, top);
        $display("%b %b", x[0], top[0]);
        $display("%b %b", x[1:0], top[1:0]);
        $display("%b %b", x[0+:1], top[0+:1]);
    end
endmodule
