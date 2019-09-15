module foo(clock, data);
    input clock;
    output reg [54:0] data;
    initial data[11*4] = 0;
    always @(clock) begin : block_name
        integer i, j;
        for (i = 4; i >= 0; i--) begin
            for (j = 9; j >= 0; j--) begin
                data[11*(4-i) + j + 1] = data[11*(4-i) + j];
            end
            if (i != 0)
                data[11*(4-i) + 0] = data[11*(4-(i-1)) + 10];
        end
        data[11*4] = ~data[11*4];
    end
endmodule

module top;
    wire [54:0] data;
    reg clock;
    foo f(clock, data);

    initial begin
        clock = 1;
        forever #1 clock = ~clock;
    end

    initial begin : foo
        $monitor("%d %b", $time, data);
        #100;
        $finish();
    end
endmodule
