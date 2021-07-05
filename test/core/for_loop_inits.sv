module top;
    initial begin
        integer x, y;
        x = 0;
        y = 3;
        for (x += 1; x < y; x++)
            $display("A x = %0d", x);
    end
    initial begin
        integer x, y;
        x = 0;
        for (x += 1, y = 3; x < y; x++)
            $display("B x = %0d", x);
    end
    initial begin
        integer x, y;
        x = 0;
        for (y = 3, x += 1; x < y; x++)
            $display("C x = %0d", x);
    end
    initial
        for (integer x = 0, y = 3; x < y; x++)
            $display("D x = %0d", x);
    initial
        for (integer x = 0, byte y = 3; x < y; x++)
            $display("E x = %0d", x);
    initial begin
        integer x, y;
        x = 0;
        for (x++, y = 3; x < y; x++)
            $display("F x = %0d", x);
    end
    initial begin
        integer x, y;
        x = 0;
        for (y = 3, x++; x < y; x++)
            $display("G x = %0d", x);
    end
    initial begin
        integer x, y;
        x = 0;
        for (--x, y = 3; x < y; x++)
            $display("H x = %0d", x);
    end
    initial begin
        integer x, y;
        x = 0;
        for (y = 3, --x; x < y; x++)
            $display("I x = %0d", x);
    end
    initial begin
        integer x, y;
        x = 0;
        y = 2;
        for (--y, ++y, y++, ++x, --x, --x; x < y; x++)
            $display("J x = %0d", x);
    end
endmodule
