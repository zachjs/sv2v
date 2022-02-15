module mod(input clk);
    logic x, y, z;
    initial begin
        $display(x, y, z);
        z = 1;
        x = @(posedge y or posedge clk) z;
        $display(x, y, z);
    end
endmodule
