`default_nettype none

module top;

    reg [7:0] a, b;
    wire [7:0] result;

    Device dut(
        .a(a),
        .b(b),
        .result(result)
    );

    initial begin
        $monitor($time, " %b | %b = %b", a, b, result);
        {a, b} = 16'h0;
        #10 {a, b} = 16'h0102;
        #10 {a, b} = 16'hff00;
        #10 {a, b} = 16'h00ff;
        #10 {a, b} = 16'hf0f0;
        #10 {a, b} = 16'h0ff0;
        #10 $finish;
    end

endmodule
