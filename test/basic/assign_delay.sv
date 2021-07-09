module top;
    reg x;
    wire y;
    assign #5 y = x;
    initial begin
        #1 x = 0;
        #1 x = 1;
        #20 x = 0;
        #20 x = 1;
    end
endmodule
