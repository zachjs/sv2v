module top;
    reg flag;
    wire [1:0] out;
    Example example(flag, out);
    initial begin
        $monitor("%2d %b %b", $time, flag, out);
        #1 flag = 0;
        #1 flag = 1;
        #1 flag = 0;
        #1 flag = 1;
    end
endmodule
