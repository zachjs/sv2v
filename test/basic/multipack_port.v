module Producer(clock, data);
    parameter INIT = 0;
    input clock;
    output reg [54:0] data;
    initial data[11*4] = INIT;
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
    reg clock;
    initial begin
        clock = 1;
        repeat (100)
            #1 clock = ~clock;
    end

    wire [54:0] foo;
    Producer #(.INIT(0)) p1(clock, foo);

    wire [109:0] bar;
    Producer #(.INIT(0)) p2(clock, bar[54:0]);
    Producer #(.INIT(1)) p3(clock, bar[109:55]);

    initial
        $monitor("%d %b %b", $time, foo, bar);
endmodule
