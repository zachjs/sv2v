module top;
    initial begin : blk1
        integer x, y, z;
        y = 10;
        z = 101;
        for (x = 0; x < y; {x, y, z} = {x + 32'd1, y - 32'd2, z >> 1})
            $display("x = %0d, y = %0d, z = %0d", x, y, z);
    end
    initial begin : blk2
        integer x;
        for (x = 0; x < 3; x = x + 1)
            $display("x = %0d", x);
    end
endmodule
