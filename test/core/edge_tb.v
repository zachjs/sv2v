module top;
    reg clk;
    initial begin
        clk = 0;
        repeat (10)
            #5 clk = ~clk;
    end
    mod m(clk);
endmodule
