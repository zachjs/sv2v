module top;
    reg [2:0] test;
    reg [3:0] foo;
    reg [3:0] bar;

    initial begin
        test = 0;
        $display(test);
        foo = 4'b0011;
        $display(foo);
        bar = 4'b1111;
        $display(bar);
    end
endmodule
