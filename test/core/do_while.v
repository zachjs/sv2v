module top;
    integer x = 0;
    initial
        while (x < 3) begin
            $display("hi %0d", x);
            x++;
            if (x != 2)
                $display("step");
        end
endmodule
