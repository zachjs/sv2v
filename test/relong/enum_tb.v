`default_nettype none

module top;


    reg rawMode;
    wire [1:0] rawOperation;

    Example dut(
        .rawMode(rawMode),
        .rawOperation(rawOperation)
    );

    initial begin
        $monitor($time, " rawMode: %b rawOperation: %b", rawMode, rawOperation);
        rawMode = 1'b0;
        #10 rawMode = 1'b1;
        #10 rawMode = 1'b0;
        #10 $finish;
    end

endmodule
