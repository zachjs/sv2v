module top;
    initial #2 begin
        $display("M %0d %0d", 0, 0);
        $display("M %0d %0d", 10, 1);
        $display("M %0d %0d", 11, 2);

        $display("N %b %b", 1'b1, 32'd42);
        $display("N %b %b", 1'b0, 32'd42);
        $display("N %b %b", 1'b1, 32'd2);
        $display("N %b %b", 8'd18, 32'd42);
        $display("N %b %b", 8'd18, 16'd26);

        $display("I %b %b", 1'b1, 32'd42);
        $display("I %b %b", 1'b0, 32'd42);
        $display("I %b %b", 1'b1, 32'd2);
        $display("I %b %b", 8'd18, 32'd42);
        $display("I %b %b", 8'd18, 16'd26);
    end
endmodule
