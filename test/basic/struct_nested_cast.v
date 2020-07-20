module top;
    reg w;
    initial begin
        w <= 1'b1;
        $display("%b", w);
    end
endmodule
