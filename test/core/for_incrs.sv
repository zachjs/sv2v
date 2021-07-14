module top;
    initial
        for (integer x = 0, y = x + 10, z = y * 10 + 1; x < y; x += 1, y -= 2, z >>= 1)
            $display("x = %0d, y = %0d, z = %0d", x, y, z);
    initial
        for (integer x = 0; x < 3; ) begin
            $display("x = %0d", x);
            ++x;
        end
endmodule
