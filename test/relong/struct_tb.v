`default_nettype none

module top;

    reg clock, clear;
    wire success;

    Example dut(
        .clock(clock),
        .clear(clear),
        .success(success)
    );

    initial begin
        clock = 1;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time, " success: %b data_out: %h mode_out: %h", success, dut.data_out, dut.mode_out);
        clear = 1'b1;
        repeat(3) @(posedge clock);
        clear = 1'b0;
        repeat(20) @(posedge clock);
        $finish;
    end

endmodule
