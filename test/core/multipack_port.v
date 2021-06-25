module Producer(clock, data);
    parameter INIT = 0;
    parameter CHUNKS = 5;
    input clock;
    output reg [11*CHUNKS - 1:0] data;
    initial data[11*(CHUNKS - 1)+:11] = INIT;
    always @(clock) begin : block_name
        integer i, j;
        for (i = (CHUNKS-1); i >= 0; i--) begin
            for (j = 9; j >= 0; j--) begin
                data[11*((CHUNKS-1)-i) + j + 1] = data[11*((CHUNKS-1)-i) + j];
            end
            if (i != 0)
                data[11*((CHUNKS-1)-i) + 0] = data[11*((CHUNKS-1)-(i-1)) + 10];
        end
        data[11*(CHUNKS-1)] = ~data[11*(CHUNKS-1)];
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
    Producer #(.INIT(0), .CHUNKS(3)) p2(clock, bar[109-:33]);
    Producer #(.INIT(1), .CHUNKS(1)) p3(clock, bar[76-:11]);
    Producer #(.INIT(2), .CHUNKS(1)) p4(clock, bar[55+:11]);
    Producer #(.INIT(3)) p5(clock, bar[54-:55]);

    initial
        $monitor("%d %b %b", $time, foo, bar);
endmodule
