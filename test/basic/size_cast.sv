module top;
    localparam BW = 3;
    logic [2:0] test;
    logic [3:0] foo;
    logic [3:0] bar;
    integer x;
    reg [7:0] y;

    initial begin
        test = BW'(0);
        $display(test);
        foo = 2'('1);
        $display(foo);
        bar = $bits(bar)'('1);
        $display(bar);
        x = 1'('1); $display("%b %0d", x, x);
        y = 1'('1); $display("%b %0d", y, y);
        x = 2'('0); $display("%b %0d", x, x);
        y = 2'('0); $display("%b %0d", y, y);
        x = 2'('1); $display("%b %0d", x, x);
        y = 2'('1); $display("%b %0d", y, y);
        x = 2'('x); $display("%b %0d", x, x);
        y = 2'('x); $display("%b %0d", y, y);
        x = 2'('z); $display("%b %0d", x, x);
        y = 2'('z); $display("%b %0d", y, y);
    end
endmodule
