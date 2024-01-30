module top;
    initial begin
        $display("[%0t] Info:", $time);
        $display("[%0t] Info: ", $time, "%b", 1);
        $display("[%0t] Warning:", $time);
        $display("[%0t] Warning: ", $time, "%b", 2);
        $display("[%0t] Error:", $time);
        $display("[%0t] Error: ", $time, "%b", 3);
        $display("[%0t] Fatal:", $time);
        $finish;
    end
endmodule
