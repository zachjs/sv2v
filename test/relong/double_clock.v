`default_nettype none

module Device(
    input wire clock, clear,
    output wire [3:0] data
);

    SharedMemory memory(
        .clock1(clock),
        .clock2(clock),
        .clear(clear), // Verilog doesn't support inferred ports
        .data1(data[1:0]),
        .data2(data[3:2])
    );

endmodule

module SharedMemory(
    input wire clock1, clock2, clear,
    output reg [1:0] data1, data2
);

    reg [3:0] memory;

    // Just a dumb example to generate interesting values
    always @(posedge clock1) begin
        if(clear)
            memory <= 4'b0;
        else
            memory <= {~memory[2:0], 1'b0};
    end

    always @(posedge clock1)
        data1 <= memory[1:0];

    always @(posedge clock2) begin
        data2 <= memory[3:2];
    end


endmodule