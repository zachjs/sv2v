module top;
    initial begin
        $display("A %0d", 1);
        $display("B %0d", 2);
        $display("C %0d", 4);
        $display("B %0d", 2);
        $display("A %0d", 1);
    end
endmodule
