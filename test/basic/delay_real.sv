module top;
    reg x, y;
    initial begin
        $monitor($time, x, y);
        #1 x = 0;
        #1 x = 1;
        #0.75 y = 0;
        #0.75 y = 1;
    end
endmodule
