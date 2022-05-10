module top;
    reg [1:0] a, b;
    wire [1:0] c, d;
    initial begin
        $monitor("%2d %b %b %b %b", $time, a, b, c, d);
        #1 force c = 1;
        #1 release c;
        #1 force c = b;
        #1 force d = a;
        #1 release c;
        #1 assign a = 1;
        #1 assign a = 3;
        #1 assign b = 2;
        #1 a = 0;
        #1 deassign a;
        #1 a = 0;
        #1 release d;
    end
endmodule
