module top;
    initial begin
        $display("%b", 5'b01010);
        $display("%b", 3'o7);
        $display("%b", 8'hab);
        $display("%b", 8'd11);
        $display("%b", 8'dx___);
        $display("%b", 8'dz___);
    end
endmodule
