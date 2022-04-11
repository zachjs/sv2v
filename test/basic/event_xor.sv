module top;
    reg x, y;
    always @(x ^ y)
        $display("%d %b %b", $time, x, y);
    initial begin
        #1 {x, y} = 2'b00;
        #1 {x, y} = 2'b01;
        #1 {x, y} = 2'b10;
        #1 {x, y} = 2'b11;
        #1 {x, y} = 2'b00;
        #1 {x, y} = 2'b10;
    end
endmodule
