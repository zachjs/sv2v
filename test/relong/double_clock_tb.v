`default_nettype none

module top;

    reg clock, clear;
    wire [3:0] data;

    Device dut(
        .clock(clock),
        .clear(clear),
        .data(data)
    );

    initial begin
        clock = 1;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time, " data: %h", data);
        clear = 1'b1;
        repeat(3) @(posedge clock);
        clear = 1'b0;
        repeat(20) @(posedge clock);
        $finish;
    end

endmodule
