module top;
    reg a, b, c, d;

    initial begin
        fork
            #1 a = 1;
            wait(a);
        join
        $display("a %0d", $time);
    end

    initial begin
        fork
            b = 1;
            #1 wait(b);
        join
        $display("b %0d", $time);
    end

    initial begin
        fork
            #1 wait(c) $display("c done %0d", $time);
            #1 wait(d) $display("d done %0d", $time);
            begin
                #1 c = 1;
                #1 d = 1;
            end
        join
        $display("cd %0d", $time);
    end
endmodule
