module top;
    integer i;
    byte b;
    shortint s;
    initial begin
        $monitor("%2d %b %b %b", $time, i, b, s);

        #1 i = (b = (s = 0));
        #1 i = 1;
        #1 i = {1'bx, b++, 1'bx};
        #1 i = {1'bx, b--, 1'bx};
        #1 i = {1'bx, ++b, 1'bx};
        #1 i = {1'bx, --b, 1'bx};

        #1 i = 3;
        while (--i) begin
            if (i == 2) begin
                s++;
                #10;
            end
            else if (i == 1) begin
                b++;
                #3 continue;
                $display("UNREACHABLE");
            end
            b--;
            #1;
        end

        #1;
        for (i[i++] = s--; b++ - 10 != s--; i[++i]++) begin
            #1;
            if (i & 1)
                continue;
            #10;
        end
    end
endmodule
