`default_nettype none

module top;

    reg [8:0] dataIn;
    wire [7:0] dataOut;
    reg clock, clear;


    CacheWithInterface dut(
        .dataIn(dataIn[7:0]),
        .dataOut(dataOut),
        .clock(clock),
        .clear(clear)
    );

    initial begin
        clock = 1;
        forever #5 clock = ~clock;
    end

    reg [7:0] last;
    initial begin
        $monitor($time, " %h -> %h [%h]", dataIn, dataOut, last);
        clear = 1'b1;
        last = 8'h0;
        dataIn = 8'h0;
        repeat (3) @(posedge clock);
        clear = 1'b0;
        for (dataIn = 8'h0; dataIn <= 9'hff; dataIn = dataIn + 8'h1) begin
            @(posedge clock);
            if (~dataOut != last)
                $error("Mismatch");
            last = last + 8'h1;
        end
        $finish;
    end
endmodule

