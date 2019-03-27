`default_nettype none

module top;

    reg [1:0] select;
    // This is actually a 3x4-bit array, but must be flattened for Verilog
    wire [11:0] data;

    Example dut(
        .select(select),
        .data(data)
    );

    reg [2:0] i; // This needs to be wider than select
    initial begin
        $monitor($time, " %d = {%h}", select, data);
        select = 2'd0;
        for(i = 0; i <= 2'd3; i = i + 3'd1) begin
            #10 select = i; // Drop upper bits
        end
        #10 $finish;
    end

endmodule
