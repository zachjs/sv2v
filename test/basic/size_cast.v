module top;
    reg [2:0] test;
    reg [3:0] foo;
    reg [3:0] bar;
    integer x;
    reg [7:0] y;

    initial begin
        test = 0;
        $display(test);
        foo = 4'b0011;
        $display(foo);
        bar = 4'b1111;
        $display(bar);
        x = 1'b1; $display("%b %0d", x, x);
        y = 1'b1; $display("%b %0d", y, y);
        x = 2'b00; $display("%b %0d", x, x);
        y = 2'b00; $display("%b %0d", y, y);
        x = 2'b11; $display("%b %0d", x, x);
        y = 2'b11; $display("%b %0d", y, y);
        x = 2'bxx; $display("%b %0d", x, x);
        y = 2'bxx; $display("%b %0d", y, y);
        x = 2'bzz; $display("%b %0d", x, x);
        y = 2'bzz; $display("%b %0d", y, y);
    end
endmodule
