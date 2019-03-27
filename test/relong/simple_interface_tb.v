`default_nettype none

module top;

    reg [7:0] dataIn;
    wire [31:0] dataOut;

    reg clock, clear;

    Device dut(
        .dataIn(dataIn),
        .dataOut(dataOut),
        .clock(clock),
        .clear(clear)
    );

    // Just some random test bench code to make sure it works as expected
    initial begin
        clock = 1;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time," dataIn: %h dataOut: %h shift: %b", dataIn, dataOut, dut.consumer.local_shift);
        clear <= 1'b1;
        dataIn <= 8'h0;
        repeat(5) @(posedge clock);
        clear <= 1'b0;
        @(posedge clock);
        dataIn <= 8'h44;
        @(posedge clock);
        dataIn <= 8'h77;
        @(posedge clock);
        dataIn <= 8'h11;
        @(posedge clock);
        dataIn <= 8'h0;
        repeat(5) @(posedge clock);
        $finish;
    end

endmodule
