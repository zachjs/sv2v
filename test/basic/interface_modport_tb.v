module top;
    reg clock;
    initial begin
        clock = 1;
        forever #5 clock = ~clock;
    end

    initial begin
        repeat(30)
            @(posedge clock);
        $finish;
    end

    Tester #(1) t1(clock);
    Tester #(2) t2(clock);
    Tester #(3) t3(clock);
    Tester #(4) t4(clock);
endmodule
