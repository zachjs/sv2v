`default_nettype none

module top;

    reg clock, clear;
    reg [7:0] dataIn;
    wire check1, check2;
    wire [63:0] checkData;


    Example dut(
        .clock(clock),
        .clear(clear),
        .dataIn(dataIn),
        .check1(check1),
        .check2(check2),
        .checkData(checkData)
    );

    initial begin
        clock = 1;
        forever #5 clock = ~clock;
    end

    initial begin
        $monitor($time, " data: %h check: %b checkData: %h", dataIn, {check1, check2}, checkData);
        clear = 1'b1;
        dataIn = 8'h0;
        repeat(3) @(posedge clock);
        clear = 1'b0;
        @(posedge clock);
        dataIn = 8'haa;
        repeat(20) @(posedge clock);
        $finish;
    end

endmodule
