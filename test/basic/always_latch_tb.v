module top;
    wire a;
    reg b;
    reg en;

    initial begin
        en = 1;
        forever #1 en = ~en;
    end

    test m(.a, .b, .en);

    initial begin
        $monitor($time, a, b, en);
        #1; b = 1;
        #1; b = 0;
        #1; b = 0;
        #1; b = 1;
        #1; b = 0;
        $finish;
    end
endmodule
