module top;
    // The below blocks must be named when converted to Verilog-2005 because it
    // contains a data declaration.
    initial begin
        integer i;
        i = 1;
        $display("%08d", i);
    end
    initial begin
        integer i;
        i = 1;
        $display("%08d", i);
    end
endmodule
