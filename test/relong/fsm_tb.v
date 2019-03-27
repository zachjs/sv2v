`default_nettype none

module top;

    reg clock, clear;
    reg a;
    wire x;

    FSM dut(
        .clock(clock),
        .clear(clear),
        .a(a),
        .x(x)
    );

    initial begin
        clock = 1;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time, " a: %b x: %b state: %h", a, x, dut.currentState);
        clear = 1'b1;
        a = 1'b0;
        repeat(3) @(posedge clock);
        clear = 1'b0;
        a = 1'b1;
        repeat(5) @(posedge clock);
        a = 1'b0;
        repeat(5) @(posedge clock);
        a = 1'b1;
        @(posedge clock);
        a = 1'b0;
        @(posedge clock);
        a = 1'b1;
        @(posedge clock);
        a = 1'b1;
        @(posedge clock);
        a = 1'b0;
        @(posedge clock);
        $finish;
    end

endmodule
