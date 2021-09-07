module top;
    reg a;
    wire b;
    reg [7:0] c;
    wire [31:0] d;
    mod m(a, b, c, d);
    initial begin
        $monitor("%2d %b %b %b %b", $time, a, b, c, d);
        #1 a = 0;
        #1 a = 1;
        for (c = 0; c < 10; c = c + 1)
            #1;
    end
endmodule
