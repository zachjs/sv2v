module top;
    reg clock;
    initial begin
        clock = 0;
        repeat (100)
            #1 clock = ~clock;
    end

    reg clear;
    initial clear = 0;

    reg data;
    initial data = 0;

    Module m(clock, clear, data);
    initial m.hello;
endmodule
