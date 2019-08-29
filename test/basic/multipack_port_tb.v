module top;
    wire [0:54] data;
    reg clock;
    foo f(clock, data);

    initial begin
        clock = 1;
        forever #1 clock = ~clock;
    end

    initial begin : foo
        $monitor("%d %b", $time, data);
        #100;
        $finish();
    end
endmodule
