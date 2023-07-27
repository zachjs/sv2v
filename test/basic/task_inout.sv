module top;
    task t(inout [7:0] x, y);
        begin
            x = ~x;
            if (y) begin
                x = x * 3;
                y = y + 1;
            end
        end
    endtask
    reg [7:0] a, b;
    always @* t(a, b);
    initial begin
        a = 0;
        b = 1;
        $monitor("%d %b %b", $time, a, b);
        repeat (100)
            #5 b = b * 3 - 1;
    end
endmodule
