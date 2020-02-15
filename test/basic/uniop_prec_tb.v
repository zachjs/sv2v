module top;
    reg [1:0] a;
    wire b;
    Example example(a, b);
    initial begin
        $monitor("%2d %b %b", $time, a, b);
        #1;
        #1; a[0] = 1;
        #1; a[1] = 1;
        #1; a[0] = 1'sbx;
    end
endmodule
