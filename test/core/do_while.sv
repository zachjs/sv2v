module top;
    integer x = 0;
    initial
        do begin
            $display("hi %0d", x);
            x++;
            if (x == 2)
                continue;
            $display("step");
        end while (0 < x && x < 3);
endmodule
