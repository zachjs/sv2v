module top;
    initial $display("Elaboration Info:");
    initial $display("Elaboration Info: ", "%b", 1);
    initial $display("Elaboration Warning:");
    initial $display("Elaboration Warning: ", "%b", 2);
    initial $display("Elaboration Error:");
    initial $display("Elaboration Error: ", "%b", 3);
    initial begin
        $display("Elaboration Fatal:");
        $finish;
    end
    initial begin
        $display("Elaboration Fatal:");
        $finish(0);
    end
    initial begin
        $display("Elaboration Fatal: ", "%b", 4);
        $finish(0);
    end
endmodule
