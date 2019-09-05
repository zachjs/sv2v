module top;
    initial
        for (logic [1:0] a = 0; a < 3; a++) begin
            if (a inside {2'b01, 2'b00})
                $display("fizz");
            if (a inside {2'b10})
                $display("buzz");
        end
endmodule
