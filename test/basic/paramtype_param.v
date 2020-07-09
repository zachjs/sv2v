module top;
    localparam FORMAT = "%s %0d %0d";
    initial begin
        $display(FORMAT, "A", 64, 64);
        $display(FORMAT, "B", 32, 32);
        $display(FORMAT, "C", 16, 16);
        $display(FORMAT, "D", 32, 32);
        $display(FORMAT, "E", 8, 8);
    end
endmodule
