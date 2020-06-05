module top;
    initial begin
        $display("%b", 3'b000);
        $display("%b", 3'b001);
    end

    initial begin
        $display("%b", 5'b1);
        $display("%b", 5'b1);
        $display("%b", 32'b1);
    end
endmodule
