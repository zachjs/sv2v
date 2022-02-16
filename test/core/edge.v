module mod(input clk);
    always @(posedge clk)
        $display($time, "posedge");
    always @(negedge clk)
        $display($time, "negedge");
    always @(posedge clk or negedge clk)
        $display($time, "edge");
endmodule
