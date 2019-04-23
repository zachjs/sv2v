`default_nettype none

module top;

    wire [3:0] a;
    wire [31:0] result;

    Example dut(
        .a(a),
        .all(result)
    );

    reg [4:0] _a;
    assign a = _a[3:0];
    initial begin
        $monitor($time, "a: %b result: %h", a, result);
        for (_a = 0; _a < 5'hf; _a = _a + 1) begin
            #10;
        end
        $finish;
    end


endmodule