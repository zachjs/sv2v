module top;
    localparam BW = 3;
    logic [2:0] test;
    logic [3:0] foo;
    logic [3:0] bar;

    initial begin
        test = BW'(0);
        $display(test);
        foo = 2'('1);
        $display(foo);
        bar = $bits(bar)'('1);
        $display(bar);
    end
endmodule
