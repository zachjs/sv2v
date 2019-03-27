`default_nettype none

module top;

    reg [3:0] data_in;
    wire [31:0] data_out;

    Example dut(
        .data_in(data_in),
        .data_out(data_out)
    );

    reg [4:0] i; // More bits than data_in
    initial begin
        $monitor($time, " data_in: %h data_out: %h", data_in, data_out);
        data_in = 4'h0;
        for(i = 5'b0; i <= 4'hf; i = i + 4'd1)
            #10 data_in = i;
        #10 $finish;
    end

endmodule
