module top;
    reg [1:0] w;
    initial begin
        w <= 2'b00;
        $display("%b", w);
    end
endmodule
