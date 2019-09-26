module top;
    parameter WIDTH = 32;
    initial begin
        logic [31:0] w = 1234;
        int x = -235;
        int y = 1234;
        logic [4:0] z = y;
        $display("%0d %0d", w, 5'(w));
        $display("%0d %0d", x, 5'(x));
        $display("%0d %0d", y, 5'(y));
        $display("%0d %0d", z, 5'(z));
        $display("%0d %0d", w+1, 5'(w+1));
        $display("%0d %0d", x+1, 5'(x+1));
        $display("%0d %0d", y+1, 5'(y+1));
        $display("%0d %0d", z+1, 5'(z+1));
        $display("%b %b", w, 40'(w));
        $display("%b %b", x, 40'(x));
        $display("%b %b", y, 40'(y));
        $display("%b %b", z, 40'(z));
        $display("%0d %0d", w, ($clog2(WIDTH))'(w));
        $display("%0d %0d", x, ($clog2(WIDTH))'(x));
        $display("%0d %0d", y, ($clog2(WIDTH))'(y));
        $display("%0d %0d", z, ($clog2(WIDTH))'(z));
    end
    localparam bit foo = '0;
    localparam logic [31:0] bar = 32'(foo);
    initial $display("%b %b", foo, bar);
endmodule
