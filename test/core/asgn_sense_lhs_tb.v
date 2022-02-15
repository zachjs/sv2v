module top;
    reg clk;
    mod m(clk);
    initial begin
        $dumpvars(0, m);
        clk = 0;
        repeat (10)
            #5 clk = ~clk;
    end
endmodule
