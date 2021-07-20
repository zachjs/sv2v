module top;
    reg clk;
    initial begin
        clk = 0;
        forever
            #1 clk = ~clk;
    end

    reg [7:0] row, col;
    wire [47:0] flat;
    mod m(clk, row, col, flat);

    integer i, j;
    initial begin
        $monitor("%3d %0d %0d %b", $time, row, col, flat);
        repeat (10)
            for (row = 0; row < 2; row = row + 1)
                for (col = 0; col < 3; col = col + 1)
                    #2;
        $finish;
    end
endmodule
