`default_nettype none

module top;

    reg [4:0] __input;
    wire [3:0] result;

    Example dut(
        .a(__input[3:2]),
        .b(__input[1:0]),
        .result(result)
    );


    initial begin
        $monitor($time, "i: %b result: %b", __input, result);
        for (__input = 0; __input < 5'hf; __input = __input + 1) begin
            #10;
        end
        $finish;
    end


endmodule