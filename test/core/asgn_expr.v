module top;
    integer i;
    reg signed [7:0] b;
    reg signed [15:0] s;
    initial begin
        $monitor("%2d %b %b %b", $time, i, b, s);

        #1;
        s = 0;
        b = 0;
        i = 0;

        #1 i = 1;

        #1;
        b = b + 1;
        i = {1'bx, b - 8'b1, 1'bx};

        #1;
        b = b - 1;
        i = {1'bx, b + 8'b1, 1'bx};

        #1;
        b = b + 1;
        i = {1'bx, b, 1'bx};

        #1;
        b = b - 1;
        i = {1'bx, b, 1'bx};

        #1;
        i = 3;
        i = i - 1;
        s = s + 1;
        #10;
        b = b - 1;
        #1;
        i = i - 1;
        b = b + 1;
        #3;
        i = i - 1;

        #1;
        i = i + 1;
        s = s - 1;
        i[i] = s;
        b = b + 1;
        s = s - 1;
        while (b - 1 - 10 != s + 1) begin
            #1;
            if (!(i & 1))
                #10;
            i = i + 1;
            i[i] = i[i] + 1;
            b = b + 1;
            s = s - 1;
        end
    end
endmodule
