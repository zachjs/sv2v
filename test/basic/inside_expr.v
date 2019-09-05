module top;
    initial begin : foo
        reg [1:0] a;
        for (a = 0; a < 3; a++) begin
            if (a == 2'b01 || a == 2'b00)
                $display("fizz");
            if (a == 2'b10)
                $display("buzz");
        end
    end
endmodule
